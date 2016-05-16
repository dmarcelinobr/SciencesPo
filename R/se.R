#' @encoding UTF-8
#' @title Calculates the Standard Error of the Mean
#'
#' @description Computes the standard error of the sample mean.
#'
#' @param x  An \R object.
#' @param na.rm A logical value indicating whether \code{NA}
#' should be stripped before the computation proceeds.
#' Default is \code{na.rm=TRUE}.
#' @param \dots Additional arguements (currently ignored)
#'
#' @details The standard error of the mean (SEM) (\emph{assuming statistical independence of the values in the sample}) is estimated by taking the standard deviation of the population sample, divided by the square root of the sample size: \deqn{se = \frac{{s}}{{\sqrt{n}}}}
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @seealso \code{\link{CI}}, \code{\link{CV}}, \code{\link{skewness}}, \code{\link{kurtosis}}, \code{\link{WinsorizedMean}}, \code{\link{Outliers}}
#' @examples
#' x <- c(1, 2.3, 2, 3, 4, 8, 12, 43, -1,-4)
#' myse <- sd(x)/sqrt(length(x))
#' myse
#' # With the 'se' function:
#' SE(x)
#' @export
#' @rdname SE
`SE` <- function(x, na.rm = TRUE, ...) UseMethod("SE")

#' @rdname SE
#' @export
`SE.default` <- function(x, na.rm = TRUE, ...) {
  if (!is.numeric(x) && !is.complex(x) && !is.logical(x) && !is.vector(x)) stop ("The argument should be a numeric vector.")
  if (na.rm) x <- x[!is.na(x)] else if(any(is.na(x))) return(x[FALSE][NA])
  ans <- sqrt(var(x)/length(x))
  return(ans)
}

#' @rdname SE
#' @export
`SE.data.frame` <- function(x, na.rm = TRUE, ...) sapply(x, SE)
NULL
