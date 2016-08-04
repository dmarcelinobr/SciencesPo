#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List Condorcet() {
  CharacterVector x = CharacterVector::create( "Name", "Rank", "Method" ) ;
  NumericVector y   = NumericVector::create( 0.0, 1.0, 2.0) ;
  List z            = List::create( x, y ) ;
  return z ;
  }

