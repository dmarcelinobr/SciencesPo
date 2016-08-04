#' @title Condorcet Voting
#' @description Condorcet Voting.
#' @export
#' @importFrom Rcpp evalCpp
Condorcet <- function() {
  .Call('SciencesPoCondorcet', PACKAGE = 'SciencesPo')
}
