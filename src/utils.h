#ifndef sd_utils_h
#define sd_utils_h

/* integer recycling macro  */
#define RECYCLE(X,Y) ( (X) == (Y) ? 0 : (X) )

#define MIN(X,Y) ((X) < (Y) ? (X) : (Y))
#define MAX(X,Y) ((X) > (Y) ? (X) : (Y))
#define ABS(X) ((X)<0 ? -1*(X) : (X))

unsigned int max_length(SEXP);

/* Get element from SEXP list and determine some parameters.
 *
 * Input:
 * x: A list of integer vectors or a character vector
 * i: Index in x: what element to extract.
 * bytes: (boolean) if (bytes) then x is assumed to be a character vector.
 * 
 * Output:
 * len  : the length of the i'th object in x.
 * isna : wether the i'th object represents an NA
 * c    : if (bytes) then c will contain the values of the i'th element in x, coerced to integers.
 *
 * Return value: 
 * A pointer to the integer representation of the i'th object in x.
 *
 */
unsigned int *get_elem(SEXP x, int i, int bytes, int *len, int *isna, unsigned int *c);

#endif