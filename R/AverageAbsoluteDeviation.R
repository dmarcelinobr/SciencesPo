#' @encoding UTF-8
#' @title Average Absolute Deviation
#'
#' @description Calculates the average (mean) absolute deviation from the sample mean.
#' @param x	A numeric vector containing the observations.
#' @param na.rm A logical value for \code{na.rm}, default is \code{na.rm=TRUE}.
#' @param \dots Additional arguements (currently ignored)

#' @details Proposed by Garrett, the "Mean Absolute Deviation"--or "Average Deviation (AD)" is "the mean of the deviation of all the separate scores in the series taken from their mean (occasionally from the median or mode)", (1971, p. 481). The statistical literature has not yet adopted a standard notation for the "Mean Absolute Deviation" and the "Median Absolute Deviation". As a result, both statistics have been denoted as "MAD", which may lead to confusion once they may produce different values.
#' The R \code{\link[stats]{mad}} by default computes the "Median Absolute Deviation"; Thus, to obtain the "Mean Absolute Deviation", one has to use \code{mad(x, constant = 1)}.
#' The function \code{\link[SciencesPo]{AverageAbsoluteDeviation}} will calculate the "Mean Absolute Deviation"--or "Average Deviation (AD)" right way.
#'
#' @references
#' Garrett, Henry (1982) \emph{Statistics in Psychology and Education}. 6th, Paragon.
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @keywords Exploratory
#' @seealso \code{\link[stats]{mad}}
#' @examples
#' x <- c(15, 10, 6, 8, 11)
#' AverageAbsoluteDeviation(x)
#' mad(x) # median
#'
#' @rdname AverageAbsoluteDeviation
#' @export
`AverageAbsoluteDeviation` <- function(x, na.rm = TRUE, ...)
  UseMethod("AverageAbsoluteDeviation")

#' @rdname AverageAbsoluteDeviation
#' @export
`AverageAbsoluteDeviation.default` <- function(x, na.rm = TRUE, ...) {
  if (!is(x, "numeric") & !is(x, "integer")) {
    stop("\"x\" must be numeric")
  }
  if (!is(na.rm, "logical") | length(na.rm) != 1) {
    stop("\"na.rm\" must be a single logical value")
  }
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  ans <- mean(abs(x - mean(x)))
  return(ans)

}## -- end of AverageAbsoluteDeviation
NULL
