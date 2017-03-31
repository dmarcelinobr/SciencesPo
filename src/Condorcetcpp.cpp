#include <Rcpp.h>
using namespace Rcpp;
//
//' @title Condorcet Voting
//'
//' @description  Condorcet Voting.
//'
//' @importFrom Rcpp evalCpp
//' @export
//' @examples
//'
//' voters <- structure(
//' list(Candidates = structure(c(5L, 1L, 2L, 3L, 6L, 4L),
//' .Label = c("Albert", "Bruce", "Charles", "Edward", "Mary",
//' "Rose"), class = "factor"),
//' Vote_A = c(1, 2, 3, 4, 5, 6),
//' Vote_B = c(2, 4, 1, 5, 6, 3),
//' Vote_C = c(6, 5, 3, 2, 4, 1),
//' Vote_D = c(3, 6, 4, 1, 5, 2),
//' Vote_E = c(2, 5, 1, 4, 3, 6),
//' Vote_F = c(1, 4, 2, 6, 5, 3),
//' Vote_G = c(3, 1, 6, 4, 2, 5),
//' Vote_H = c(4, 2, 5, 1, 6, 3)),
//' .Names = c("Candidates", "Vote_A", "Vote_B", "Vote_C",
//' "Vote_D", "Vote_E", "Vote_F", "Vote_G", "Vote_H"),
//' row.names = c(NA, -6L), class = "data.frame")
//'
//'
// [[Rcpp::export]]
List Condorcet() {
  CharacterVector x = CharacterVector::create( "Name", "Rank", "Method" ) ;
  NumericVector y   = NumericVector::create( 0.0, 1.0, 2.0) ;
  List z            = List::create( x, y ) ;
  return z ;
}



//' @title Condorcet Voting
//'
//' @description Condorcet Voting.
//' @param votes a data frame or matrix.
//' @importFrom Rcpp evalCpp
//' @export
//'
// [[Rcpp::export]]
IntegerMatrix PairCount(IntegerMatrix votes) {
  int Num_Candidates = votes.nrow();
  int Num_Ballots = votes.ncol();
  IntegerMatrix Pairwise(Num_Candidates, Num_Candidates);
  for (int CurCand = 0; CurCand < Num_Candidates; CurCand++) {
    IntegerVector CandRank = votes(CurCand, _);
    IntegerMatrix Pref_Cur_Cand(Num_Candidates, Num_Ballots);
    for (int i = 0; i < Num_Candidates; i++) {
      for (int j = 0; j < Num_Ballots; j++) {
        Pref_Cur_Cand(i, j) = votes(i, j) - CandRank(j);
      }
    }
    for (int i = 0; i < Num_Candidates; i++) {
      int G0 = 0;
      for (int j = 0; j < Num_Ballots; j++) {
        if (Pref_Cur_Cand(i, j) > 0) G0 += 1;
      }
      Pairwise(CurCand, i) = G0;
    }
  }
  return(Pairwise);
}




//' @title Condorcet Voting
//'
//' @description Condorcet Voting.
//' @param pairs a matrix of pairs.
//' @importFrom Rcpp evalCpp
//' @export
//'
// [[Rcpp::export]]
IntegerMatrix Schulze(IntegerMatrix pairs) {
  int nrow = pairs.nrow();
  IntegerMatrix Schulze(nrow, nrow);
  for (int i = 0; i < nrow; i++) {
    for (int j = 0; j < nrow; j++) {
      if (i != j) {
        if (pairs(i, j) > pairs(j, i)) {
          Schulze(i, j) = pairs(i, j);
        } else {
          Schulze(i, j) = 0;
        }
      }
    }
  }
  for (int i = 0; i < nrow; i++) {
    for (int j = 0; j < nrow; j++) {
      if (i != j) {
        for (int k = 0; k < nrow; k++) {
          if ((i != k) && (j != k)) {
            Schulze(j, k) = (std::max)(Schulze(j, k), (std::min)(Schulze(j, i), Schulze(i, k)));
          }
        }
      } else {
        if ((i = j)) {
          Schulze(i, j) = 0;
        }
      }
    }
  }
  return(Schulze);
}
