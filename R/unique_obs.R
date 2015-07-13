
#' Number of unique elements in a vector.
#'
#' A wrapper around \code{length(unique(x))}
#'
#' @param x vector
#' @param ... passed to \code{\link{unique}}
#' @keywords Describe
#' @examples
#' x<-sample(1000, rep=TRUE)
#'  uniqueObs(x)
#' @export
uniqueObs <- function(x, ...) {
  if (is.factor(x)) {
    length(levels(x))
  } else {
    length(unique(x, ...))
  }
}
