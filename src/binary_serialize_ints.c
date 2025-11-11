// src/binary_serialize_ints_opt.c

#define R_NO_REMAP
#define STRICT_R_HEADERS

#include <R.h>
#include <Rinternals.h>

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#ifdef DEBUG
#define _DP(...) \
    Rprintf("[DEBUG %s:%d] ", __FILE__, __LINE__), \
    Rprintf(__VA_ARGS__), \
    Rprintf("\n")
#else
#define _DP(...)
#endif

#if 1 // TODO: Remove for release
#define ASSERT(expr)                                                                            \
    do                                                                                          \
    {                                                                                           \
        if (!(expr))                                                                            \
        {                                                                                       \
            fprintf(stderr, "Assertion failed: %s, line %d (%s)\n", __FILE__, __LINE__, #expr); \
            __asm__("int3; nop");                                                               \
        }                                                                                       \
    } while (0)
#define BP ASSERT(!"Break point");
#endif

typedef struct
{
    uint8_t *data;
    size_t size;
    size_t capacity;
} buffer_t;

static void buf_init(buffer_t *b)
{
    b->data = NULL;
    b->size = 0;
    b->capacity = 0;
}

static int buf_reserve(buffer_t *b, size_t new_size)
{
    if (new_size > b->capacity)
    {
        size_t new_cap = (b->capacity ? b->capacity * 2 : 64);
        while (new_cap < new_size)
            new_cap *= 2;
        uint8_t *new_data = realloc(b->data, new_cap);
        if (!new_data)
            return 0; // allocation failed ENUM instead of 0
        b->data = new_data;
        b->capacity = new_cap;
    }
    return 1;
}

static int buf_append(buffer_t *b, const void *src, size_t n)
{
    if (!buf_reserve(b, b->size + n))
        return 0; // fail // allocation failed ENUM instead of 0
    memcpy(b->data + b->size, src, n);
    b->size += n; 
    return 1;
}

SEXP binary_serialize_ints_C(SEXP x)
{
    if (TYPEOF(x) != INTSXP)
        perror("binary_serialize_ints_C: input must be an integer vector");

    R_xlen_t n = XLENGTH(x);

    SEXP out = PROTECT(Rf_allocVector(RAWSXP, n * 4));
    memcpy(RAW(out), INTEGER(x), (size_t)n * 4);

    UNPROTECT(1);
    return out;
}

static SEXP getListElement(SEXP list, const char *str)
{
    SEXP names = Rf_getAttrib(list, R_NamesSymbol);
    for (R_len_t i = 0; i < LENGTH(list); i++)
    {
        if (strcmp(CHAR(STRING_ELT(names, i)), str) == 0)
        {
            return VECTOR_ELT(list, i);
        }
    }
    return R_NilValue;
}

// Consider removing consts if they do not catch errors

SEXP binary_serialize_filter_data_C(SEXP x)
{
    int32_t n_protected = 0;

    const char *header = "FILTDATA"; // Designated initializer for headers
    const int32_t version = 1;

    buffer_t buf_;
    buffer_t *buf = &buf_;
    buf_init(buf);

    buf_append(buf, header, strlen(header)+1); // pragma va a misa
    buf_append(buf, &version, sizeof(version)); 

    SEXP dataset_lists = PROTECT(getListElement(x, "dataset_lists")); n_protected++; 
    const int dataset_lists_len = Rf_length(dataset_lists); // Can go into header
    buf_append(buf, &dataset_lists_len, sizeof(dataset_lists_len));

    _DP("DL_L %d", dataset_lists_len);

    for (int dataset_list_idx = 0; dataset_list_idx < dataset_lists_len; dataset_list_idx++)
    {
        SEXP dataset_list_element = PROTECT(VECTOR_ELT(dataset_lists, dataset_list_idx)); n_protected++;
        const char *dataset_list_name = R_CHAR(STRING_ELT(getListElement(dataset_list_element, "name"), 0));
        SEXP dataset_list = getListElement(dataset_list_element, "dataset_list");
        int32_t dataset_list_name_len = strlen(dataset_list_name) + 1;
        int32_t dataset_list_len = Rf_length(dataset_list);

        buf_append(buf, &dataset_list_name_len, sizeof(dataset_list_name_len));
        buf_append(buf, dataset_list_name, dataset_list_name_len);
        buf_append(buf, &dataset_list_len, sizeof(dataset_list_len));
        _DP("DL_N %s", dataset_list_name);
        for (int dataset_idx = 0; dataset_idx < dataset_list_len; dataset_idx++)
        {
            SEXP dataset_element = PROTECT(VECTOR_ELT(dataset_list, dataset_idx)); n_protected++;
            const char *dataset_name = R_CHAR(STRING_ELT(getListElement(dataset_element, "name"), 0));
            _DP("DS_N %s", dataset_name);
            int32_t dataset_name_len = strlen(dataset_name) + 1;
            SEXP variables = getListElement(dataset_element, "variables");
            int32_t dataset_nvar = Rf_length(variables);            
            int32_t dataset_nrow = INTEGER(getListElement(dataset_element, "nrow"))[0];
            buf_append(buf, &dataset_name_len, sizeof(dataset_name_len)); //TODO: create alias
            buf_append(buf, dataset_name, dataset_name_len);
            buf_append(buf, &dataset_nrow, sizeof(dataset_nrow));
            buf_append(buf, &dataset_nvar, sizeof(dataset_nvar));
            for(int variable_idx = 0; variable_idx < dataset_nvar; variable_idx++)
            {
                SEXP variable_element = PROTECT(VECTOR_ELT(variables, variable_idx)); n_protected++;
                const char *variable_name = R_CHAR(STRING_ELT(getListElement(variable_element, "name"), 0));
                _DP("VAR_N %s", variable_name);
                int32_t variable_name_len = strlen(variable_name) + 1;
                const char *variable_label = R_CHAR(STRING_ELT(getListElement(variable_element, "label"), 0));
                int32_t variable_label_len = strlen(variable_label) + 1;
                const char *variable_class = R_CHAR(STRING_ELT(getListElement(variable_element, "class"), 0));
                int32_t variable_class_len = strlen(variable_class) + 1;
                const char *variable_kind = R_CHAR(STRING_ELT(getListElement(variable_element, "kind"), 0));
                int32_t variable_kind_len = strlen(variable_kind) + 1;
                int32_t NA_count = INTEGER(getListElement(variable_element, "NA_count"))[0];                
                buf_append(buf, &variable_name_len, sizeof(variable_name_len));
                buf_append(buf, variable_name, variable_name_len);
                buf_append(buf, &variable_label_len, sizeof(variable_label_len));
                buf_append(buf, variable_label, variable_label_len);
                buf_append(buf, &variable_class_len, sizeof(variable_class_len));
                buf_append(buf, variable_class, variable_class_len);
                buf_append(buf, &variable_kind_len, sizeof(variable_kind_len));
                buf_append(buf, variable_kind, variable_kind_len);
                buf_append(buf, &NA_count, sizeof(NA_count));
                // TODO: Write some helpers

                if(!strcmp(variable_kind, "categorical")) {                    
                    SEXP values = getListElement(variable_element, "value");  // STRSXP
                    int32_t *count = INTEGER(getListElement(variable_element, "count"));
                    int32_t length = Rf_length(values); // Unlikely to reach the limits of length, we want a 32 bit for serialization
                    buf_append(buf, &length, sizeof(length));
                    for(int value_idx = 0; value_idx < length; value_idx++) {
                        const char *value = CHAR(STRING_ELT(values, value_idx));
                        int32_t value_str_len = strlen(value) + 1;
                        buf_append(buf, &value_str_len, sizeof(value_str_len));
                        buf_append(buf, value, value_str_len);                                                                                    
                    }                    
                    buf_append(buf, count, sizeof(int32_t) * length);
                } else if(!strcmp(variable_kind, "numerical")) {
                    double min = REAL(getListElement(variable_element, "min"))[0];
                    double max = REAL(getListElement(variable_element, "max"))[0];
                    double *density = REAL(getListElement(variable_element, "density"));
                    int32_t density_length = Rf_length(getListElement(variable_element, "density")); // Unlikely to reach the limits of length, we want a 32 bit for serialization

                    buf_append(buf, &min, sizeof(min));
                    buf_append(buf, &max, sizeof(max));
                    buf_append(buf, &density_length, sizeof(density_length));
                    buf_append(buf, density, sizeof(double)*density_length);
                } else if(!strcmp(variable_kind, "date")) {
                    double min = REAL(getListElement(variable_element, "min"))[0];
                    double max = REAL(getListElement(variable_element, "max"))[0];                    
                    buf_append(buf, &min, sizeof(min));
                    buf_append(buf, &max, sizeof(max));
                } else{
                    Rf_error("Uknown kind: %s\n", variable_kind);
                }
            }
        }        
    }

    SEXP out = PROTECT(Rf_allocVector(RAWSXP, buf_.size)); n_protected++; //TODO: PROTECT alias (define) that also increase nprotected.
    memcpy(RAW(out), buf_.data, buf_.size); // Free memory at the end
    free(buf_.data);
    UNPROTECT(n_protected);

    return out;
}

typedef uint8_t byte;

static inline void buf_read(const byte *buf, void *out, size_t n, uint32_t *offset)
{
    memcpy(out, buf + (*offset), n);
    (*offset) += n;
}

// NULL TERMINATED STRINGS

SEXP binary_deserialize_filter_data_C(SEXP x)
{

    int32_t n_protected = 0;

    const char expected_magicnum[] = "FILTDATA";
    const int32_t expected_version = 1;

    _DP("EV: %u", expected_version);
    // Rf_error("Expected version: %d\n", expected_version);
    uint32_t offset = 0;

    if (TYPEOF(x) != RAWSXP)
        Rf_error("Input must be a raw vector");

    byte *buf = RAW(x);

    char magicnum[strlen(expected_magicnum)+1];
    buf_read(buf, magicnum, strlen(expected_magicnum) + 1, &offset);
    _DP("Magicnum: %s", magicnum);

    if (strcmp(magicnum, expected_magicnum) != 0)
        Rf_error("Invalid magicnum");

    int32_t version;
    buf_read(buf, &version, sizeof(version), &offset);
    _DP("Version: %d", version);

    if (version != expected_version)
    {
        Rf_error("Expected version: %u found version: %u", expected_version, version);
    }

    int32_t dataset_lists_len;
    buf_read(buf, &dataset_lists_len, sizeof(dataset_lists_len), &offset);

    SEXP dataset_lists = PROTECT(Rf_allocVector(VECSXP, dataset_lists_len));
    n_protected++;
    for (int dataset_list_idx = 0; dataset_list_idx < dataset_lists_len; dataset_list_idx++)
    {        
        SEXP dataset_list_element = PROTECT(Rf_allocVector(VECSXP, 2)); n_protected++;
        SEXP dataset_list_element_names = PROTECT(Rf_allocVector(STRSXP, 2)); n_protected++;

        int32_t dataset_list_name_len;
        buf_read(buf, &dataset_list_name_len, sizeof(dataset_list_name_len), &offset);
        _DP("DSLN_LEN: %d", dataset_list_name_len);

        char *dataset_list_name = malloc(dataset_list_name_len);
        buf_read(buf, dataset_list_name, dataset_list_name_len, &offset);
        _DP("DSLN_name: %s", dataset_list_name);

        int32_t dataset_list_len;
        buf_read(buf, &dataset_list_len, sizeof(dataset_list_len), &offset);
        _DP("DSL_LENGTH: %d", dataset_list_len);

        SEXP dataset_list = PROTECT(Rf_allocVector(VECSXP, dataset_list_len)); n_protected++;

        for (int dataset_idx = 0; dataset_idx < dataset_list_len; dataset_idx++)
        {
            SEXP dataset_element = PROTECT(Rf_allocVector(VECSXP, 3)); n_protected++;
            SEXP dataset_element_names = PROTECT(Rf_allocVector(STRSXP, 3)); n_protected++;            

            int32_t dataset_name_len;
            buf_read(buf, &dataset_name_len, sizeof(dataset_name_len), &offset);
            _DP("DSN_LEN: %d", dataset_name_len);

            char *dataset_name = malloc(dataset_name_len); // Free replace by static array with a sensible size
            buf_read(buf, dataset_name, dataset_name_len, &offset);
            _DP("DSN_name: %s", dataset_name);

            int32_t dataset_nrow;
            buf_read(buf, &dataset_nrow, sizeof(dataset_nrow), &offset);
            _DP("DS_NROW: %d", dataset_nrow);

            int32_t dataset_nvar;
            buf_read(buf, &dataset_nvar, sizeof(dataset_nvar), &offset);
            _DP("DS_NVAR: %d", dataset_nvar);

            SEXP variables = PROTECT(Rf_allocVector(VECSXP, dataset_nvar)); n_protected++;

            for(int variable_idx = 0; variable_idx < dataset_nvar; variable_idx++)
            {
                _DP("Processing variable: %d", variable_idx);
                SEXP variable_element;
                SEXP variable_element_names;
                int32_t variable_name_len;
                buf_read(buf, &variable_name_len, sizeof(variable_name_len), &offset);
                _DP("VN_L: %d", variable_name_len);
                char *variable_name = malloc(variable_name_len);                
                buf_read(buf, variable_name, variable_name_len, &offset);
                _DP("VN: %s", variable_name);

                int32_t variable_label_len;
                buf_read(buf, &variable_label_len, sizeof(variable_label_len), &offset);
                _DP("VL_L: %d", variable_label_len);
                char *variable_label = malloc(variable_label_len);
                buf_read(buf, variable_label, variable_label_len, &offset);
                _DP("VL: %s", variable_label);

                int32_t variable_class_len;
                buf_read(buf, &variable_class_len, sizeof(variable_class_len), &offset);
                _DP("VC_L: %d", variable_class_len);
                char *variable_class = malloc(variable_class_len);
                buf_read(buf, variable_class, variable_class_len, &offset);
                _DP("VC: %s", variable_class);

                int32_t variable_kind_len;
                buf_read(buf, &variable_kind_len, sizeof(variable_kind_len), &offset);
                _DP("VK_L: %d", variable_kind_len);
                char *variable_kind = malloc(variable_kind_len);
                buf_read(buf, variable_kind, variable_kind_len, &offset);
                _DP("VK: %s", variable_kind);
                
                int32_t NA_count;
                buf_read(buf, &NA_count, sizeof(NA_count), &offset);
                SEXP R_NA_count = PROTECT(Rf_allocVector(INTSXP, 1)); n_protected++;
                INTEGER(R_NA_count)[0] = NA_count;

                if(!strcmp(variable_kind, "categorical")) {
                    _DP("Processing categorical");                    
                    variable_element = PROTECT(Rf_allocVector(VECSXP, 7)); n_protected++;
                    variable_element_names = PROTECT(Rf_allocVector(STRSXP, 7)); n_protected++;

                    int32_t values_len;
                    buf_read(buf, &values_len, sizeof(values_len), &offset);

                    SEXP R_values = PROTECT(Rf_allocVector(STRSXP, values_len)); n_protected++;
                    for(int value_idx = 0; value_idx < values_len; value_idx++) {
                        int32_t value_len;
                        buf_read(buf, &value_len, sizeof(value_len), &offset);
                        char * value;
                        value = malloc(value_len);
                        buf_read(buf, value, value_len, &offset);

                        SET_STRING_ELT(R_values, value_idx, Rf_mkChar(value));                                                                                                            
                    }

                    SEXP R_count = PROTECT(Rf_allocVector(INTSXP, values_len)); n_protected++;
                    buf_read(buf, INTEGER(R_count), sizeof(int32_t) * values_len, &offset);

                    SET_VECTOR_ELT(variable_element, 5, R_values);
                    SET_STRING_ELT(variable_element_names, 5, Rf_mkChar("value"));
                    SET_VECTOR_ELT(variable_element, 6, R_count);
                    SET_STRING_ELT(variable_element_names, 6, Rf_mkChar("count"));

                } else if(!strcmp(variable_kind, "numerical")) {
                    _DP("Processing numerical");
                    variable_element = PROTECT(Rf_allocVector(VECSXP, 8)); n_protected++;
                    variable_element_names = PROTECT(Rf_allocVector(STRSXP, 8)); n_protected++;

                    double min;
                    buf_read(buf, &min, sizeof(min), &offset);
                    SEXP R_min = PROTECT(Rf_allocVector(REALSXP, 1)); n_protected++;
                    REAL(R_min)[0] = min;
                    _DP("MIN: %f", min);

                    double max;
                    buf_read(buf, &max, sizeof(max), &offset);
                    SEXP R_max = PROTECT(Rf_allocVector(REALSXP, 1)); n_protected++;
                    REAL(R_max)[0] = max;
                    _DP("MAX: %f", max);

                    int32_t density_len;
                    buf_read(buf, &density_len, sizeof(density_len), &offset);
                    _DP("DL: %d", density_len);

                    SEXP R_density = PROTECT(Rf_allocVector(REALSXP, density_len)); n_protected++;
                    buf_read(buf, REAL(R_density), sizeof(double) * density_len, &offset);


                    SET_VECTOR_ELT(variable_element, 5, R_min);
                    SET_STRING_ELT(variable_element_names, 5, Rf_mkChar("min"));
                    SET_VECTOR_ELT(variable_element, 6, R_max);
                    SET_STRING_ELT(variable_element_names, 6, Rf_mkChar("max"));
                    SET_VECTOR_ELT(variable_element, 7, R_density);
                    SET_STRING_ELT(variable_element_names, 7, Rf_mkChar("density"));

                } else if(!strcmp(variable_kind, "date")) {
                    _DP("Processing date");
                    variable_element = PROTECT(Rf_allocVector(VECSXP, 7)); n_protected++;
                    variable_element_names = PROTECT(Rf_allocVector(STRSXP, 7)); n_protected++;

                    double min;
                    buf_read(buf, &min, sizeof(min), &offset);
                    SEXP R_min = PROTECT(Rf_allocVector(REALSXP, 1)); n_protected++;
                    REAL(R_min)[0] = min;
                    _DP("MIN: %f", min);

                    double max;
                    buf_read(buf, &max, sizeof(max), &offset);
                    SEXP R_max = PROTECT(Rf_allocVector(REALSXP, 1)); n_protected++;
                    REAL(R_max)[0] = max;
                    _DP("MAX: %f", max);

                    SET_VECTOR_ELT(variable_element, 5, R_min);
                    SET_STRING_ELT(variable_element_names, 5, Rf_mkChar("min"));
                    SET_VECTOR_ELT(variable_element, 6, R_max);
                    SET_STRING_ELT(variable_element_names, 6, Rf_mkChar("max"));
                } else{
                    Rf_error("Uknown kind: %s", variable_kind);
                }

                SET_VECTOR_ELT(variable_element, 0, Rf_mkString(variable_name));
                SET_STRING_ELT(variable_element_names, 0, Rf_mkChar("name"));
                SET_VECTOR_ELT(variable_element, 1, Rf_mkString(variable_label));
                SET_STRING_ELT(variable_element_names, 1, Rf_mkChar("label"));
                SET_VECTOR_ELT(variable_element, 2, Rf_mkString(variable_class));
                SET_STRING_ELT(variable_element_names, 2, Rf_mkChar("class"));
                SET_VECTOR_ELT(variable_element, 3, Rf_mkString(variable_kind));
                SET_STRING_ELT(variable_element_names, 3, Rf_mkChar("kind"));
                SET_VECTOR_ELT(variable_element, 4, R_NA_count);
                SET_STRING_ELT(variable_element_names, 4, Rf_mkChar("NA_count"));
                Rf_setAttrib(variable_element, R_NamesSymbol, variable_element_names);
                SET_VECTOR_ELT(variables, variable_idx, variable_element);
            }
            
            SET_VECTOR_ELT(dataset_element, 0, Rf_mkString(dataset_name));
            SET_STRING_ELT(dataset_element_names, 0, Rf_mkChar("name"));

            SEXP R_nrow = PROTECT(Rf_allocVector(INTSXP, 1)); n_protected++;
            INTEGER(R_nrow)[0] = dataset_nrow;
            SET_VECTOR_ELT(dataset_element, 1, R_nrow);
            SET_STRING_ELT(dataset_element_names, 1, Rf_mkChar("nrow"));

            SET_VECTOR_ELT(dataset_element, 2, variables);
            SET_STRING_ELT(dataset_element_names, 2, Rf_mkChar("variables"));
            
            
            Rf_setAttrib(dataset_element, R_NamesSymbol, dataset_element_names);
            Rf_setAttrib(dataset_list, R_NamesSymbol, R_NilValue);
            SET_VECTOR_ELT(dataset_list, dataset_idx, dataset_element);

            free(dataset_name);
        }
        
        SET_VECTOR_ELT(dataset_list_element, 0, Rf_mkString(dataset_list_name));
        SET_STRING_ELT(dataset_list_element_names, 0, Rf_mkChar("name"));
        SET_VECTOR_ELT(dataset_list_element, 1, dataset_list);
        SET_STRING_ELT(dataset_list_element_names, 1, Rf_mkChar("dataset_list"));
        Rf_setAttrib(dataset_list_element, R_NamesSymbol, dataset_list_element_names);
        free(dataset_list_name);
        

        SET_VECTOR_ELT(dataset_lists, dataset_list_idx, dataset_list_element);
    }

    // SEXP out = PROTECT(Rf_allocVector(VECSXP, 3));
    // n_protected++;
    // SEXP names = PROTECT(Rf_allocVector(STRSXP, 3));
    // n_protected++;

    // SET_STRING_ELT(names, 0, Rf_mkChar("magicnum"));
    // SET_VECTOR_ELT(out, 0, Rf_mkString(expected_magicnum));

    // SEXP ver = PROTECT(Rf_allocVector(INTSXP, 1));
    // n_protected++;
    // INTEGER(ver)
    // [0] = version;
    // SET_VECTOR_ELT(out, 1, ver);
    // SET_STRING_ELT(names, 1, Rf_mkChar("version"));

    SEXP x_out = PROTECT(Rf_allocVector(VECSXP, 1));
    n_protected++;
    SEXP x_out_names = PROTECT(Rf_allocVector(STRSXP, 1));
    n_protected++;
    SET_VECTOR_ELT(x_out, 0, dataset_lists);
    SET_STRING_ELT(x_out_names, 0, Rf_mkChar("dataset_lists"));
    Rf_setAttrib(x_out, R_NamesSymbol, x_out_names);

    // SET_VECTOR_ELT(out, 2, x_out);
    // SET_STRING_ELT(names, 2, Rf_mkChar("x"));

    // Rf_setAttrib(out, R_NamesSymbol, names);
    UNPROTECT(n_protected);
    return x_out;
}

