#' @title Weighted Variance
#'
#'@description Weighted Variance Formula
#'
#'@param x the varaible.
#'@param w the variance.
#' @keywords Stats
`weighted.variance` <- function(x, w){
  return(sum(w * (x - stats::weighted.mean(x,w))^2)/((length(x)-1)*mean(w)))
}
NULL

