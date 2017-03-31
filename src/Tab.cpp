#include <Rcpp.h>
using namespace Rcpp;

//' @title Rcpp table function is faster than base R
//'
//' @description  Rcpp table function is faster than base R (3x speedup).
//' @param x
//'
//' @export
//'
// [[Rcpp::export]]
IntegerVector Tab(CharacterVector x) {
  return table(x);
}
