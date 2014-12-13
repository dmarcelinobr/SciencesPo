#define USE_RINTERNALS
#include <R.h>
#include <Rdefines.h>
#include "utils.h"
#include <ctype.h>


// Translate similar sounding consonants to numeric codes; vowels are all 
// translated to 'a' and voiceless characters (and other characters) are 
// translated to 'h'.
// Upper and lower case ASCII characters are treated as separate cases,
// avoiding the use of 'tolower' whose effect depends on locale.
static unsigned int translate_soundexBR(unsigned int c) {
  switch ( c ) {
    case 'a':
    case 'e':
    case 'i':
    case 'o':
    case 'u':
    case 'y':
    case 'A':
    case 'E':
    case 'I':
    case 'O':
    case 'U':
    case 'Y':
      return 'a'; // use 'a' to encode vowels
	case 'h':
	case 'w':
	case 'H':
	case 'W':
		return 'h';
    case 'b':
    case 'f':
    case 'p':
    case 'v':
    case 'B':
    case 'F':
    case 'P':
    case 'V':
	  return '1';
    case 'c':
    case 'g':
    case 'j':
    case 'k':
    case 'q':
    case 's':
    case 'x':
    case 'z':
    case 'C':
    case 'G':
    case 'J':
    case 'K':
    case 'Q':
    case 'S':
    case 'X':
    case 'Z':
      return '2';
    case 'd':
    case 't':
    case 'D':
    case 'T':
      return '3';
    case 'l':
    case 'L':
      return '4';
    case 'm':
    case 'n':
    case 'M':
    case 'N':
      return '5';
    case 'r':
    case 'R':
      return '6';
    case '!': // we will allow all printable ASCII characters.
    case '"':
    case '#':
    case '$':
    case '%':
    case '&':
    case '\'':
    case '(':
    case ')':
    case '*':
    case '+':
    case ',':
    case '-':
    case '.':
    case '/':
    case ':':
    case ';':
    case '<':
    case '=':
    case '>':
    case '?':
    case '@':
    case '[':
    case '\\':
    case ']':
    case '^':
    case '_':
    case '`':
    case '{':
    case '|':
    case '}':
    case '~':
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
    case ' ':
      return 'h'; // ignored characters; voiceless symbols.
    default:
      return '?'; // other characters are ignored with a warning
  }
}

// Translate a string to a soundex phonetic code
//
// str: the input string
// str_len: the length of the input string
// result: the character vector in which the soundex code is copied. This 
//  should be a vector of a least length 4.
// output: the number of non-ascii or non-printable ascii characters
// encountered during translation.
static unsigned int soundexBR(const unsigned int* str, unsigned int str_len, unsigned int* result) {
  if (!str || !result) return 0;
  if (str_len == 0) {
    unsigned int j;
    for (j = 0; j < 4; ++j) result[j] = '0';
    return 0;
  }
  unsigned int i = 0, j = 0, nfail = 0;
  unsigned int cj = translate_soundexBR(str[j]);
  // the first character is copied directly and not translated to a numerical
  // code
  if ( cj == '?' ){
    // the translated character is non-printable ASCII or non-ASCII.
    ++nfail;
    result[0] = str[0];
  } else {
    result[0] = toupper(str[0]);
  }
  //result[0] = str[0] < 128 ? toupper(str[0]) : str[0];
  for (i = 1; i < str_len && j < 3; ++i) {
    unsigned int ci = translate_soundexBR(str[i]);
    if (ci == 'a') {
      // vowels are not added to the result; but we do set the previous
      // character to the vower because two consonants with a vowel in between
      // are not merged
      cj = ci;
    } else if (ci != 'h') {
      // a consonant that is not equal to the previous consonant is added to 
      // the result
      if (ci != cj) {
        result[++j] = ci;
        cj = ci;
      }
    }
    if ( ci == '?' ){
      // the translated character is non-printable ASCII or non-ASCII.
      ++nfail;
    }
  }
  // pad with zeros
  for (++j ; j < 4; ++j) result[j] = '0';
  return nfail;
}

static double soundexBR_dist(unsigned int *a, unsigned int *b, unsigned int a_len, 
    unsigned int b_len, unsigned int *nfail) {
  const unsigned int l = 4;
  unsigned int sa[l];
  unsigned int sb[l];
  (*nfail) += soundexBR(a, a_len, sa);
  (*nfail) += soundexBR(b, b_len, sb);
  for (unsigned int i = 0; i < l; ++i) 
    if (sa[i] != sb[i]) return 1.0;
  return 0.0;
}

// ================================ R INTERFACE ===============================

static void check_fail(unsigned int nfail){
  if ( nfail > 0 ){
    warning("soundex came across %d non-printable ASCII or unknown characters."
    "\n Results may be unreliable, see ?ascii.table",nfail);
  }
}

SEXP R_soundexBR(SEXP x) {
  int n = length(x);
  int bytes = IS_CHARACTER(x);

  // when a and b are character vectors; create unsigned int vectors in which
  // the elements of and b will be copied
  unsigned int *s = NULL;
  if (bytes) {
    int ml = max_length(x);
    s = (unsigned int *) malloc(ml*sizeof(unsigned int));
    if (s == NULL) {
       free(s);
       error("Unable to allocate adequate memory");
    }
  }

  if (bytes) {
    // create output variable
    SEXP y = allocVector(STRSXP, n);
    PROTECT(y);
    // compute soundexes, skipping NA's
    unsigned int nfail = 0;
    int len_s, isna_s;
    char sndx[5];
    unsigned int sndx_int[4];
    for (int i = 0; i < n; ++i) {
      s = get_elem(x, i, bytes, &len_s, &isna_s, s);
      if (isna_s) {
        SET_STRING_ELT(y, i, R_NaString);
      } else { 
        nfail += soundexBR(s, len_s, sndx_int);
        for (unsigned int j = 0; j < 4; ++j) sndx[j] = sndx_int[j];
        sndx[4] = 0;
        SET_STRING_ELT(y, i, mkChar(sndx));
      } 
    }
    // cleanup and return
    check_fail(nfail);
    free(s);
    UNPROTECT(1);
    return y;
  } else {
    // create output variable
    SEXP y = allocVector(VECSXP, n);
    PROTECT(y);
    // compute soundexes, skipping NA's
    unsigned int nfail = 0;
    int len_s, isna_s;
    for (int i = 0; i < n; ++i) {
      s = get_elem(x, i, bytes, &len_s, &isna_s, s);
      if (isna_s) {
        SEXP sndx = allocVector(INTSXP, 1);
        PROTECT(sndx);
        INTEGER(sndx)[0] = NA_INTEGER;
        SET_VECTOR_ELT(y, i, sndx);
        UNPROTECT(1);
      } else { 
        SEXP sndx = allocVector(INTSXP, 4);
        PROTECT(sndx);
        nfail += soundexBR(s, len_s, (unsigned int *)INTEGER(sndx));
        SET_VECTOR_ELT(y, i, sndx);
        UNPROTECT(1);
      } 
    }
    // cleanup and return
    check_fail(nfail);
    UNPROTECT(1);
    return y;
  }
}


SEXP R_soundexBR_dist(SEXP a, SEXP b) {
  int na = length(a);
  int nb = length(b);
  int nt = MAX(na,nb);
  int bytes = IS_CHARACTER(a);

  // when a and b are character vectors; create unsigned int vectors in which
  // the elements of and b will be copied
  unsigned int *s = NULL, *t = NULL;
  if (bytes) {
    int ml_a = max_length(a);
    int ml_b = max_length(b);
    s = (unsigned int *) malloc((ml_a + ml_b) * sizeof(unsigned int));
    t = s + ml_a;
    if (s == NULL) {
       free(s);
       error("Unable to allocate adequate memory");
    }
  }

  // create output variable
  SEXP yy = allocVector(REALSXP, nt);
  PROTECT(yy);
  double *y = REAL(yy);

  // compute distances, skipping NA's
  unsigned int nfail = 0;
  int len_s, len_t, isna_s, isna_t;
  for (int k=0; k < nt; ++k, ++y) {
    s = get_elem(a, k % na, bytes, &len_s, &isna_s, s);
    t = get_elem(b, k % nb, bytes, &len_t, &isna_t, t);
    if (isna_s || isna_t) {
      (*y) = NA_REAL;
    } else { 
      (*y) = soundexBR_dist(s, t, len_s, len_t, &nfail);
    } 
  }
  // cleanup and return
  check_fail(nfail);
  if (bytes) free(s);
  UNPROTECT(1);
  return yy;
}

SEXP R_match_soundexBR(SEXP x, SEXP table, SEXP nomatch, SEXP matchNA) {

  int nx = length(x);
  int ntable = length(table);
  int no_match = INTEGER(nomatch)[0];
  int match_na = INTEGER(matchNA)[0];
  int bytes = IS_CHARACTER(x);

  // when a and b are character vectors; create unsigned int vectors in which
  // the elements of and b will be copied
  unsigned int *s = NULL, *t = NULL;
  if (bytes) {
    int ml_x = max_length(x);
    int ml_t = max_length(table);
    s = (unsigned int *) malloc((ml_x + ml_t) * sizeof(unsigned int));
    t = s + ml_x;
    if (s == NULL) {
       free(s);
       error("Unable to allocate enough memory");
    }
  }

  // output vector
  SEXP yy = allocVector(INTSXP, nx);
  PROTECT(yy);
  int* y = INTEGER(yy);

  int index, isna_s, isna_t, len_s, len_t;
  unsigned int nfail = 0;
  double d;
  for (int i=0; i<nx; ++i) {
    index = no_match;
    s = get_elem(x, i, bytes, &len_s, &isna_s, s);

    for (int j=0; j<ntable; ++j) {
      t = get_elem(table, j, bytes, &len_t, &isna_t, t);

      if (!isna_s && !isna_t) {        // both are char (usual case)
        d = soundexBR_dist(s, t, len_s, len_t, &nfail);
        if (d < 0.5) { // exact match as d can only take on values 0 and 1
          index = j + 1;
          break;
        } 
      } else if (isna_s && isna_t) {  // both are NA
        index = match_na ? j + 1 : no_match;
        break;
      }
    }
    y[i] = index;
  }   
  // cleanup and return
  check_fail(nfail);
  if (bytes) free(s); 
  UNPROTECT(1);
  return(yy);
}