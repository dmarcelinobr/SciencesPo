#' @title Quadratic Error
#'
#' @description Handy function to estimate the mean squared error (MSE) of an estimator. It measures the average of the squares of the ``errors'', that is, the difference between the estimator and what is estimated.
#' @param observed is the vector of observed values.
#' @param estimate  is the vector of n predictions.
#'
#' @export
meanSquaredError <- function(observed, estimate){
  cat("Quadratic-Error = ", sum((observed[] - estimate[])^2), "\n")
}
