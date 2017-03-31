#include <Rcpp.h>
using namespace Rcpp;

//' @title A c++ implementation of the tabMerge function
//'
//' @description Description
//' @param hsum
//'
//' @export
//'
// [[Rcpp::export]]
NumericVector tabMerge(List hsum) {

    RCPP_UNORDERED_MAP<std::string,double> out ;

    int n = hsum.size() ;

    for(int i = 0; i < n; i++){
      NumericVector x = hsum[i] ;
      CharacterVector names = x.attr("names") ;
      int m = x.size() ;

      for(int j = 0; j < m; j++){
        String name = names[j] ;
        out[ name ] += x[j] ;
      }
    }
  return wrap(out) ;
}
