// src/binary_serialize_ints_opt.c

#define R_NO_REMAP
#define STRICT_R_HEADERS

#include <R.h>
#include <Rinternals.h>
#include <time.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>

#ifdef DEBUG
#define _DP(...) \
    Rprintf("[DEBUG %s:%d] ", __FILE__, __LINE__), \
    Rprintf(__VA_ARGS__), \
    Rprintf("\n")
#else
#define _DP(...)
#endif

// Base 0 index as opposed to R
#define DATASET_LISTS_IDX 0
#define NAME_IDX 0
#define DATASET_LIST_IDX 1
#define LABEL_IDX 1
#define NROW_IDX 2
#define VARIABLES_IDX 3
#define CLASS_IDX 2
#define KIND_IDX 3
#define NA_COUNT_IDX 4
#define VALUE_IDX 5
#define COUNT_IDX 6
#define MIN_IDX 5
#define MAX_IDX 6
#define DENSITY_IDX 7


#define MAX_STRING_LENGTH 4096
static char __buf_dataset_list_name[MAX_STRING_LENGTH];
static char __buf_dataset_name[MAX_STRING_LENGTH];
static char __buf_dataset_label[MAX_STRING_LENGTH];
static char __buf_variable_name[MAX_STRING_LENGTH];
static char __buf_variable_label[MAX_STRING_LENGTH];
static char __buf_variable_class[MAX_STRING_LENGTH];
static char __buf_variable_kind[MAX_STRING_LENGTH];
static char __buf_value[MAX_STRING_LENGTH];

#define MIN_BUFFER_SIZE 4096

typedef uint8_t byte_t;

#define COUNTED_PROTECT(x) (n_protected++, PROTECT(x)) // Forces the name n_protected to be declared in the scope

typedef enum {
    ALLOCATION_SUCCESS,
    ALLOCATION_FAIL
} allocation_result;

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
    byte_t *data;
    size_t size;
    size_t capacity;
} buffer_t;

static void buf_init(buffer_t *b)
{
    b->data = NULL;
    b->size = 0;
    b->capacity = 0;
}

static void buf_reserve(buffer_t *b, size_t new_size)
{
    if (new_size > b->capacity)
    {
        size_t new_cap = (b->capacity ? b->capacity * 2 : MIN_BUFFER_SIZE);
        while (new_cap < new_size)
            new_cap *= 2;
        uint8_t *new_data = realloc(b->data, new_cap);
        if (!new_data)
            Rf_error("Could not allocate new buffer capacity");            
        b->data = new_data;
        b->capacity = new_cap;
    }    
}

static void buf_append(buffer_t *b, const void *src, size_t n)
{
    buf_reserve(b, b->size + n);
    memcpy(b->data + b->size, src, n);
    b->size += n;
}

static void buf_append_str(buffer_t *b, const char *str) {
    int32_t str_size = strlen(str) + 1;
    buf_append(b, &str_size, sizeof(str_size));
    buf_append(b, str, str_size);    
}

static void buf_read(const byte_t *buf, void *out, size_t n, uint32_t *offset)
{
    memcpy(out, buf + (*offset), n);
    (*offset) += n;
}

static void buf_read_str(const byte_t *buf, void *out, uint32_t *offset)
{
    int32_t str_size;
    buf_read(buf, &str_size, sizeof(str_size), offset);
    buf_read(buf, out, str_size, offset);
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

    SEXP dataset_lists = COUNTED_PROTECT(VECTOR_ELT(x, DATASET_LISTS_IDX));
    const int dataset_lists_len = Rf_length(dataset_lists); // Can go into header
    buf_append(buf, &dataset_lists_len, sizeof(dataset_lists_len));

    _DP("DL_L %d", dataset_lists_len);

    for (int dataset_list_idx = 0; dataset_list_idx < dataset_lists_len; dataset_list_idx++)
    {
        SEXP dataset_list_element = COUNTED_PROTECT(VECTOR_ELT(dataset_lists, dataset_list_idx)); 
        const char *dataset_list_name = R_CHAR(STRING_ELT(VECTOR_ELT(dataset_list_element, NAME_IDX), 0));
        SEXP dataset_list = VECTOR_ELT(dataset_list_element, DATASET_LIST_IDX);
        
        int32_t dataset_list_len = Rf_length(dataset_list);

        buf_append_str(buf, dataset_list_name);        
        buf_append(buf, &dataset_list_len, sizeof(dataset_list_len));

        _DP("DL_N %s", dataset_list_name);
        for (int dataset_idx = 0; dataset_idx < dataset_list_len; dataset_idx++)
        {
            SEXP dataset_element = COUNTED_PROTECT(VECTOR_ELT(dataset_list, dataset_idx)); 
            const char *dataset_name = R_CHAR(STRING_ELT(VECTOR_ELT(dataset_element, NAME_IDX), 0));
            _DP("DS_N %s", dataset_name);            
            const char *dataset_label = R_CHAR(STRING_ELT(VECTOR_ELT(dataset_element, LABEL_IDX), 0));
            SEXP variables = VECTOR_ELT(dataset_element, VARIABLES_IDX);
            int32_t dataset_nvar = Rf_length(variables);            
            int32_t dataset_nrow = INTEGER(VECTOR_ELT(dataset_element, NROW_IDX))[0];
            buf_append_str(buf, dataset_name);            
            buf_append_str(buf, dataset_label); 
            buf_append(buf, &dataset_nrow, sizeof(dataset_nrow));
            buf_append(buf, &dataset_nvar, sizeof(dataset_nvar));
            for(int variable_idx = 0; variable_idx < dataset_nvar; variable_idx++)
            {
                SEXP variable_element = COUNTED_PROTECT(VECTOR_ELT(variables, variable_idx)); 
                const char *variable_name = R_CHAR(STRING_ELT(VECTOR_ELT(variable_element, NAME_IDX), 0));
                _DP("VAR_N %s", variable_name);
                const char *variable_label = R_CHAR(STRING_ELT(VECTOR_ELT(variable_element, LABEL_IDX), 0));
                const char *variable_class = R_CHAR(STRING_ELT(VECTOR_ELT(variable_element, CLASS_IDX), 0));
                const char *variable_kind = R_CHAR(STRING_ELT(VECTOR_ELT(variable_element, KIND_IDX), 0));
                int32_t NA_count = INTEGER(VECTOR_ELT(variable_element, NA_COUNT_IDX))[0];                                
                buf_append_str(buf, variable_name);                
                buf_append_str(buf, variable_label);                
                buf_append_str(buf, variable_class);                
                buf_append_str(buf, variable_kind);                
                buf_append(buf, &NA_count, sizeof(NA_count));                

                if(!strcmp(variable_kind, "categorical")) {                    
                    SEXP values = VECTOR_ELT(variable_element, VALUE_IDX);  // STRSXP
                    int32_t *count = INTEGER(VECTOR_ELT(variable_element, COUNT_IDX));
                    int32_t length = Rf_length(values); // Unlikely to reach the limits of length, we want a 32 bit for serialization
                    buf_append(buf, &length, sizeof(length));
                    for(int value_idx = 0; value_idx < length; value_idx++) {
                        const char *value = CHAR(STRING_ELT(values, value_idx));
                        buf_append_str(buf, value);                        
                    }                    
                    buf_append(buf, count, sizeof(int32_t) * length);
                } else if(!strcmp(variable_kind, "numerical")) {
                    double min = REAL(VECTOR_ELT(variable_element, MIN_IDX))[0];
                    double max = REAL(VECTOR_ELT(variable_element, MAX_IDX))[0];
                    double *density = REAL(VECTOR_ELT(variable_element, DENSITY_IDX));
                    int32_t density_length = Rf_length(VECTOR_ELT(variable_element, DENSITY_IDX)); // Unlikely to reach the limits of length, we want a 32 bit for serialization

                    buf_append(buf, &min, sizeof(min));
                    buf_append(buf, &max, sizeof(max));
                    buf_append(buf, &density_length, sizeof(density_length));
                    buf_append(buf, density, sizeof(double)*density_length);
                } else if(!strcmp(variable_kind, "date")) {
                    double min = REAL(VECTOR_ELT(variable_element, MIN_IDX))[0];
                    double max = REAL(VECTOR_ELT(variable_element, MAX_IDX))[0];                    
                    buf_append(buf, &min, sizeof(min));
                    buf_append(buf, &max, sizeof(max));
                } else{
                    // Rf_warning("Unknown kind: %s\n", variable_kind);
                }
            }
        }        
    }

    SEXP out = COUNTED_PROTECT(Rf_allocVector(RAWSXP, buf_.size));
    memcpy(RAW(out), buf_.data, buf_.size); // Free memory at the end
    free(buf_.data);
    UNPROTECT(n_protected);

    return out;
}

// TODO: NULL TERMINATED STRINGS

SEXP binary_deserialize_filter_data_C(SEXP x)
{

    int32_t n_protected = 0;

    const char expected_magicnum[] = "FILTDATA";
    const int32_t expected_version = 1;

    _DP("EV: %u", expected_version);
    uint32_t offset = 0;

    if (TYPEOF(x) != RAWSXP)
        Rf_error("Input must be a raw vector");

    byte_t *buf = RAW(x);

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

    SEXP dataset_lists = COUNTED_PROTECT(Rf_allocVector(VECSXP, dataset_lists_len));
    
    for (int dataset_list_idx = 0; dataset_list_idx < dataset_lists_len; dataset_list_idx++)
    {        
        SEXP dataset_list_element = COUNTED_PROTECT(Rf_allocVector(VECSXP, 2));         

        buf_read_str(buf, __buf_dataset_list_name, &offset);        
        _DP("DSLN_name: %s", __buf_dataset_list_name);

        int32_t dataset_list_len;
        buf_read(buf, &dataset_list_len, sizeof(dataset_list_len), &offset);
        _DP("DSL_LENGTH: %d", dataset_list_len);

        SEXP dataset_list = COUNTED_PROTECT(Rf_allocVector(VECSXP, dataset_list_len)); 

        for (int dataset_idx = 0; dataset_idx < dataset_list_len; dataset_idx++)
        {
            SEXP dataset_element = COUNTED_PROTECT(Rf_allocVector(VECSXP, 4)); 

            buf_read_str(buf, __buf_dataset_name, &offset);
            _DP("DSN_name: %s", __buf_dataset_name);

            buf_read_str(buf, __buf_dataset_label, &offset);
            _DP("DSN_label: %s", __buf_dataset_label);

            int32_t dataset_nrow;
            buf_read(buf, &dataset_nrow, sizeof(dataset_nrow), &offset);
            _DP("DS_NROW: %d", dataset_nrow);

            int32_t dataset_nvar;
            buf_read(buf, &dataset_nvar, sizeof(dataset_nvar), &offset);
            _DP("DS_NVAR: %d", dataset_nvar);

            SEXP variables = COUNTED_PROTECT(Rf_allocVector(VECSXP, dataset_nvar)); 

            for(int variable_idx = 0; variable_idx < dataset_nvar; variable_idx++)
            {
                _DP("Processing variable: %d", variable_idx);
                SEXP variable_element;
                buf_read_str(buf, __buf_variable_name, &offset);
                _DP("VN: %s", __buf_variable_name);
                
                buf_read_str(buf, __buf_variable_label, &offset);
                _DP("VL: %s", __buf_variable_label);

                buf_read_str(buf, __buf_variable_class, &offset);
                _DP("VC: %s", __buf_variable_class);
          
                buf_read_str(buf, __buf_variable_kind, &offset);
                _DP("VK: %s", __buf_variable_kind);
                
                int32_t NA_count;
                buf_read(buf, &NA_count, sizeof(NA_count), &offset);
                SEXP R_NA_count = COUNTED_PROTECT(Rf_allocVector(INTSXP, 1)); //TODO: Consider writing directly as in #333
                INTEGER(R_NA_count)[0] = NA_count;

                if(!strcmp(__buf_variable_kind, "categorical")) {
                    _DP("Processing categorical");                    
                    variable_element = COUNTED_PROTECT(Rf_allocVector(VECSXP, 7)); 

                    int32_t values_len;
                    buf_read(buf, &values_len, sizeof(values_len), &offset);

                    SEXP R_values = COUNTED_PROTECT(Rf_allocVector(STRSXP, values_len)); 
                    for(int value_idx = 0; value_idx < values_len; value_idx++) {                                                
                        buf_read_str(buf, __buf_value, &offset);

                        SET_STRING_ELT(R_values, value_idx, Rf_mkChar(__buf_value));                                                                                                            
                    }

                    SEXP R_count = COUNTED_PROTECT(Rf_allocVector(INTSXP, values_len)); 
                    buf_read(buf, INTEGER(R_count), sizeof(int32_t) * values_len, &offset);

                    SET_VECTOR_ELT(variable_element, 5, R_values);
                    SET_VECTOR_ELT(variable_element, 6, R_count);

                } else if(!strcmp(__buf_variable_kind, "numerical")) {
                    _DP("Processing numerical");
                    variable_element = COUNTED_PROTECT(Rf_allocVector(VECSXP, 8)); 

                    double min;
                    buf_read(buf, &min, sizeof(min), &offset);
                    SEXP R_min = COUNTED_PROTECT(Rf_allocVector(REALSXP, 1)); 
                    REAL(R_min)[0] = min;
                    _DP("MIN: %f", min);

                    double max;
                    buf_read(buf, &max, sizeof(max), &offset);
                    SEXP R_max = COUNTED_PROTECT(Rf_allocVector(REALSXP, 1)); 
                    REAL(R_max)[0] = max;
                    _DP("MAX: %f", max);

                    int32_t density_len;
                    buf_read(buf, &density_len, sizeof(density_len), &offset);
                    _DP("DL: %d", density_len);

                    SEXP R_density = COUNTED_PROTECT(Rf_allocVector(REALSXP, density_len)); 
                    buf_read(buf, REAL(R_density), sizeof(double) * density_len, &offset);

                    SET_VECTOR_ELT(variable_element, 5, R_min);
                    SET_VECTOR_ELT(variable_element, 6, R_max);
                    SET_VECTOR_ELT(variable_element, 7, R_density);

                } else if(!strcmp(__buf_variable_kind, "date")) {
                    _DP("Processing date");
                    variable_element = COUNTED_PROTECT(Rf_allocVector(VECSXP, 7)); 

                    double min;
                    buf_read(buf, &min, sizeof(min), &offset);
                    SEXP R_min = COUNTED_PROTECT(Rf_allocVector(REALSXP, 1)); 
                    REAL(R_min)[0] = min;
                    _DP("MIN: %f", min);

                    double max;
                    buf_read(buf, &max, sizeof(max), &offset);
                    SEXP R_max = COUNTED_PROTECT(Rf_allocVector(REALSXP, 1)); 
                    REAL(R_max)[0] = max;
                    _DP("MAX: %f", max);

                    SET_VECTOR_ELT(variable_element, 5, R_min);
                    SET_VECTOR_ELT(variable_element, 6, R_max);
                } else{
                    _DP("Processing Unknown");
                    variable_element = COUNTED_PROTECT(Rf_allocVector(VECSXP, 5)); 
                    // Rf_warning("Unknown kind: %s", __buf_variable_kind);
                }

                SET_VECTOR_ELT(variable_element, 0, Rf_mkString(__buf_variable_name));
                SET_VECTOR_ELT(variable_element, 1, Rf_mkString(__buf_variable_label));
                SET_VECTOR_ELT(variable_element, 2, Rf_mkString(__buf_variable_class));
                SET_VECTOR_ELT(variable_element, 3, Rf_mkString(__buf_variable_kind));
                SET_VECTOR_ELT(variable_element, 4, R_NA_count);
                SET_VECTOR_ELT(variables, variable_idx, variable_element);
            }
            
            SET_VECTOR_ELT(dataset_element, 0, Rf_mkString(__buf_dataset_name));

            SET_VECTOR_ELT(dataset_element, 1, Rf_mkString(__buf_dataset_label));

            SEXP R_nrow = COUNTED_PROTECT(Rf_allocVector(INTSXP, 1)); 
            INTEGER(R_nrow)[0] = dataset_nrow;
            SET_VECTOR_ELT(dataset_element, 2, R_nrow);

            SET_VECTOR_ELT(dataset_element, 3, variables);
            
            SET_VECTOR_ELT(dataset_list, dataset_idx, dataset_element);

        }
        
        SET_VECTOR_ELT(dataset_list_element, 0, Rf_mkString(__buf_dataset_list_name));
        SET_VECTOR_ELT(dataset_list_element, 1, dataset_list);
        

        SET_VECTOR_ELT(dataset_lists, dataset_list_idx, dataset_list_element);
    }

    SEXP x_out = COUNTED_PROTECT(Rf_allocVector(VECSXP, 1));
    
    
    SET_VECTOR_ELT(x_out, 0, dataset_lists);

    // SET_VECTOR_ELT(out, 2, x_out);
    // SET_STRING_ELT(names, 2, Rf_mkChar("x"));

    // Rf_setAttrib(out, R_NamesSymbol, names);
    UNPROTECT(n_protected);
    return x_out;
}

SEXP has_finite_C(SEXP x) {
    double *px = REAL(x);
    int n = LENGTH(x);
    for (int i = 0; i < n; i++) {
        if (R_FINITE(px[i])) return Rf_ScalarLogical(TRUE);
    }
    return Rf_ScalarLogical(FALSE);
}

enum{
  BANK_COUNT = 4
};

void count_integer(const int32_t * restrict src, size_t n, int32_t * restrict counts, int level_count) {
    // @luis this routine considered NA to be part of level_count and this makes it compatible with yours
    level_count += 1;
                      
    // Round bank size to 32-byte alignment to avoid cache line splits
    int bank_size = (level_count + 7) & ~7;
    
    // Allocate BANK_COUNT independent banks
    int32_t *banks = (int32_t *)aligned_alloc(32, bank_size * BANK_COUNT * sizeof(int32_t));
    memset(banks, 0, bank_size * BANK_COUNT * sizeof(int32_t));

    int32_t *b0 = &banks[bank_size * 0];
    int32_t *b1 = &banks[bank_size * 1];
    int32_t *b2 = &banks[bank_size * 2];
    int32_t *b3 = &banks[bank_size * 3];

    size_t i = 0;
    for (; i + (BANK_COUNT-1) < n; i += BANK_COUNT) {
        int32_t v0 = src[i+0] & 0x7FFFFFFF;
        int32_t v1 = src[i+1] & 0x7FFFFFFF;
        int32_t v2 = src[i+2] & 0x7FFFFFFF;
        int32_t v3 = src[i+3] & 0x7FFFFFFF;

        b0[v0]++;
        b1[v1]++;
        b2[v2]++;
        b3[v3]++;
    }

    // tail
    for (; i < n; i++) {
        b0[src[i] & 0x7FFFFFFF]++;
    }

    for (int j = 0; j < level_count; j++) {
        counts[j] = b0[j] + b1[j] + b2[j] + b3[j];
    }

    free(banks);
}

SEXP count_factor_C(SEXP factor_sexp) {
    int32_t n = Rf_length(factor_sexp);    
    int32_t *codes = INTEGER(factor_sexp);
    SEXP levels = Rf_getAttrib(factor_sexp, R_LevelsSymbol);
    int32_t num_levels = Rf_length(levels);
    
    SEXP counts = PROTECT(Rf_allocVector(INTSXP, num_levels+1)); //NA in index 0
    int32_t *cnt = INTEGER(counts);
    memset(cnt, 0, num_levels * sizeof(int32_t));
    count_integer(codes, n, cnt, num_levels);

    UNPROTECT(1);
    return counts;
}

SEXP max_min_count_na_C(SEXP x) {
    R_xlen_t n = XLENGTH(x);
    double *px = REAL(x);
    double s_max = R_NegInf;
    double s_min = R_PosInf;
    R_xlen_t na_count = 0;

    for (R_xlen_t k = 0; k < n; k++) {
        if (ISNAN(px[k])) {
            na_count++;
        } else {
            if (px[k] > s_max) s_max = px[k];
            if (px[k] < s_min) s_min = px[k];
        }
    }

    SEXP result = PROTECT(Rf_allocVector(VECSXP, 3));

    SET_VECTOR_ELT(result, 0, Rf_ScalarReal(s_max));
    SET_VECTOR_ELT(result, 1, Rf_ScalarReal(s_min));
    SET_VECTOR_ELT(result, 2, Rf_ScalarInteger((int) na_count));

    UNPROTECT(1);
    return result;
}
