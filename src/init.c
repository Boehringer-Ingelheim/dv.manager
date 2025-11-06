#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP binary_serialize_ints_C(SEXP);
extern SEXP traverse_list_C(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"binary_serialize_ints_C", (DL_FUNC) &binary_serialize_ints_C, 1},
    {"traverse_list_C", (DL_FUNC) &traverse_list_C, 1},
    {NULL, NULL, 0}
};

void R_init_dv_manager(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
