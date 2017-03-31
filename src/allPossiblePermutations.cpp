#include <Rcpp.h>
using namespace Rcpp;

//' @title
//' perms
//' @description
//' This function generates all the possible permutations of the set
//' {1, 2, ..., n} (for some given n). A function removes missing values.
//' @details
//' @param
//' x a integer vector
//'
//' @return
//' a matrix with n! rows and n columns
//'
//' @family Exploratory
//' @exemples
//' allPossiblePermutations(1:3)
//' @export
// [[Rcpp::export]]

NumericMatrix allPossiblePermutations(IntegerVector x) {
  std::sort(x.begin(), x.end(), std::less<double>());
  int m = x.size();

  for (int i=m-1; i>=0; --i) {
    if (IntegerVector::is_na(x[i]))
      x.erase(i);
  }

  IntegerVector out = Rcpp::clone(x);
  int n = out.size();

  int factorial;
  for (int i=0; i<=n; ++i){
    if(i == 0) factorial = 1;
    else factorial = factorial * i;
  }

  NumericMatrix permutations(factorial, n);

  for (int j=0; j<n; ++j){
    permutations(factorial-1, j) = out[n-1-j];
  }

  for (int i=0; i<factorial-1; ++i){
    do{
      for (int j=0; j<n; ++j){
        permutations(i, j) = out[j];
      }
    } while (!std::next_permutation(out.begin(), out.end()));
  }

  return permutations;
}
