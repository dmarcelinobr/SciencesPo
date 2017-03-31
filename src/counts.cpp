#include <Rcpp.h>
using namespace Rcpp;

#include <Rcpp.h>
using namespace Rcpp;

template <typename T>
struct NACompare;

template <>
struct NACompare<int> {
  inline bool operator()(int left, int right) const {
    if (left == NA_INTEGER) return false;
    if (right == NA_INTEGER) return true;
    return left < right;
  }
};

template <>
struct NACompare<double> {
  inline bool operator()(double left, double right) const {
    bool leftNaN = (left != left);
    bool rightNaN = (right != right);
    if (leftNaN != rightNaN) {
      return leftNaN < rightNaN;
    } else {
      return left < right;
    }
  }
};

template <>
struct NACompare<String> {
  inline bool operator()(const String& left, const String& right) const {
    return left < right;
  }
};

template <typename T, typename U>
inline IntegerVector do_counts(const T& x) {
  std::map< U, int, NACompare<U> > counts;
  int n = x.size();
  for (int i=0; i < n; ++i) {
    ++counts[ x[i] ];
  }
  return wrap(counts);
}

template <>
inline IntegerVector do_counts<NumericVector, double>(const NumericVector& x) {
  std::map< double, int, NACompare<double> > counts;
  int n = x.size();
  for (int i=0; i < n; ++i) {
    ++counts[ x[i] ];
  }
  IntegerVector output = wrap(counts);

  // explicitly use R's coercion mechanism to get good names
  int m = counts.size();
  NumericVector keys = no_init(m);
  typedef std::map< double, int, NACompare<double> >::iterator MapItr;
  int i = 0;
  for (MapItr it = counts.begin(); it != counts.end(); ++it) {
    keys[i] = it->first;
    ++i;
  }
  CharacterVector names = Rf_coerceVector(keys, STRSXP);
  output.attr("names") = names;
  return output;
}


//'
//' @export
//'
// [[Rcpp::export]]
IntegerVector counts(SEXP x) {
  switch (TYPEOF(x)) {
  case REALSXP: {
    IntegerVector output = do_counts<NumericVector, double>(x);
    // fix names
    for (int i=0; i < output.size(); ++i) {
      if (CHAR(STRING_ELT(output.attr("names"), i)) == "-0") {
        SET_STRING_ELT(output.attr("names"), i, Rf_mkChar("-0"));
      }
    }
    return output;
  }
  case STRSXP: return do_counts<CharacterVector, String>(x);
  case INTSXP: return do_counts<IntegerVector, int>(x);
  case LGLSXP: {
    IntegerVector output = do_counts<LogicalVector, int>(x);
    SET_STRING_ELT( output.attr("names"), 0, Rf_mkChar("FALSE") );
    SET_STRING_ELT( output.attr("names"), 1, Rf_mkChar("TRUE") );
    return output;
  }
  default: {
    stop("unrecognized SEXP type");
    return R_NilValue;
  }
  }
}
