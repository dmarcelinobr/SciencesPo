#include <Rcpp.h>
using namespace Rcpp;

//' @title \pkg{Rcpp} table function is faster than \code{\link[base:table]{base::table}} R function.
//'
//' @description  Rcpp table function is faster than base R (3x speedup).
//' @param x
//'
//' @export
//'
// [[Rcpp::export]]
NumericVector myTab(CharacterVector x){
  IntegerVector tab = table(x);
  double n = sum(tab);
  NumericVector out(tab.size());
  for(int i = 0; i < tab.size(); ++i){
    out[i] = tab[i] / n;
  }
  return wrap(out);
}
