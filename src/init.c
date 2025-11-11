#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP binary_serialize_filter_data_C(SEXP);
extern SEXP binary_deserialize_filter_data_C(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"binary_serialize_filter_data_C", (DL_FUNC) &binary_serialize_filter_data_C, 1},
    {"binary_deserialize_filter_data_C", (DL_FUNC) &binary_deserialize_filter_data_C, 1},
    {NULL, NULL, 0}
};

void R_init_dv_manager(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
