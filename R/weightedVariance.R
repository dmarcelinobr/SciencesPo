#' Weighted variance
#'
#' @param x an object containing the values whose weighted variance is to be computed
#' @param w a numerical vector of weights the same length as x giving the weights to use for elements of x
#' @param na.rm a logical value indicating whether NA values in x should be stripped before the computation proceeds
#' @return a length-one numeric vector
#' @export
weightedVariance <- function(x, w, na.rm = FALSE) {
  if (na.rm) {
    w <- w[i <- !is.na(x)]
    x <- x[i]
  }
  sum.w <- sum(w)
  (sum(w*x^2) * sum.w - sum(w*x)^2) / (sum.w^2 - sum(w^2))
}
