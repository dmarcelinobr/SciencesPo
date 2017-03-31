// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// allPossiblePermutations
NumericMatrix allPossiblePermutations(IntegerVector x);
RcppExport SEXP SciencesPo_allPossiblePermutations(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(allPossiblePermutations(x));
    return rcpp_result_gen;
END_RCPP
}
// Condorcet
List Condorcet();
RcppExport SEXP SciencesPo_Condorcet() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(Condorcet());
    return rcpp_result_gen;
END_RCPP
}
// PairCount
IntegerMatrix PairCount(IntegerMatrix votes);
RcppExport SEXP SciencesPo_PairCount(SEXP votesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type votes(votesSEXP);
    rcpp_result_gen = Rcpp::wrap(PairCount(votes));
    return rcpp_result_gen;
END_RCPP
}
// Schulze
IntegerMatrix Schulze(IntegerMatrix pairs);
RcppExport SEXP SciencesPo_Schulze(SEXP pairsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type pairs(pairsSEXP);
    rcpp_result_gen = Rcpp::wrap(Schulze(pairs));
    return rcpp_result_gen;
END_RCPP
}
// counts
IntegerVector counts(SEXP x);
RcppExport SEXP SciencesPo_counts(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(counts(x));
    return rcpp_result_gen;
END_RCPP
}
// findMode
String findMode(IntegerVector x);
RcppExport SEXP SciencesPo_findMode(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(findMode(x));
    return rcpp_result_gen;
END_RCPP
}
// myTab
NumericVector myTab(CharacterVector x);
RcppExport SEXP SciencesPo_myTab(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(myTab(x));
    return rcpp_result_gen;
END_RCPP
}
// Tab
IntegerVector Tab(CharacterVector x);
RcppExport SEXP SciencesPo_Tab(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(Tab(x));
    return rcpp_result_gen;
END_RCPP
}
// tabMerge
NumericVector tabMerge(List hsum);
RcppExport SEXP SciencesPo_tabMerge(SEXP hsumSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type hsum(hsumSEXP);
    rcpp_result_gen = Rcpp::wrap(tabMerge(hsum));
    return rcpp_result_gen;
END_RCPP
}
