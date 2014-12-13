#define USE_RINTERNALS
#include <R.h>
#include <Rdefines.h>


unsigned int max_length(SEXP x){
  unsigned int t=0, m;
  for (int i=0; i<length(x); ++i){
    m = length(VECTOR_ELT(x,i));
    if (t < m) t = m;
  }
  return t;
}

unsigned int *get_elem(SEXP x, int i, int bytes, int *len, int *isna, unsigned int *c){
  unsigned int *out;
  if (bytes){
    *len  = length(STRING_ELT(x,i));
    *isna = ( STRING_ELT(x,i) == NA_STRING );
    for (int j=0; j < *len; j++ )
      c[j] =  CHAR(STRING_ELT(x,i))[j];
    out = c;
  } else {
    *len  = length(VECTOR_ELT(x,i));
    *isna = (INTEGER(VECTOR_ELT(x,i))[0] == NA_INTEGER);
    out = (unsigned int *) INTEGER(VECTOR_ELT(x,i));
  }
  return out;
}