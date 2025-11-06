// src/binary_serialize_ints_opt.c

#define R_NO_REMAP
#define STRICT_R_HEADERS

#include <R.h>
#include <Rinternals.h>
#include <string.h>

SEXP binary_serialize_ints_C(SEXP x) {
    if (TYPEOF(x) != INTSXP)
        perror("binary_serialize_ints_C: input must be an integer vector");

    R_xlen_t n = XLENGTH(x);

    SEXP out = PROTECT(Rf_allocVector(RAWSXP, n * 4));
    memcpy(RAW(out), INTEGER(x), (size_t)n * 4);

    UNPROTECT(1);
    return out;
}

static SEXP getListElement(SEXP list, const char *str) {
    SEXP names = Rf_getAttrib(list, R_NamesSymbol);
    for (R_len_t i = 0; i < LENGTH(list); i++) {
        if (strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
            return VECTOR_ELT(list, i);
        }
    }
    return R_NilValue;
}

SEXP traverse_list_C(SEXP x) {
    
    SEXP dataset_lists = PROTECT(getListElement(x, "dataset_lists"));    
    
    // Rprintf("Hola");

    // SEXP ln = PROTECT(Rf_allocVector(INTSXP, 1));
    // INTEGER(ln)[0] = Rf_length(elmt);

    // Rf_setAttrib(elmt, Rf_install("ln"), ln);
    // Rf_setAttrib(elmt, Rf_install("nm"), names);

    for(int i = 0; i < Rf_length(dataset_lists); i++){
        SEXP elmt = PROTECT(VECTOR_ELT(dataset_lists, i));
        SEXP names = PROTECT(Rf_getAttrib(elmt, R_NamesSymbol));
        for(int j = 0; j < Rf_length(names); j++){
            Rprintf("%s\n", CHAR(STRING_ELT(names, j)));            
        }
        UNPROTECT(2);
    }

    UNPROTECT(1);
    return dataset_lists;
}
