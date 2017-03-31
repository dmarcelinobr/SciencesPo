#' Calculate a geometric mean
#'
#' Calculate a geometric mean
#'
#' @param x An R object.
#' @param ... further arguments passed to \code{\link{prod}}
#' @export
#' @seealso \code{\link{harmonicMean}} and \code{\link{mean}}
`geometricMean` <- function(x, ...)
{
  return(prod(x, ...)^(1/length(x)))
}
NULL




#' Calculate a harmonic mean
#'
#' Calculate a harmonic mean
#'
#' @param x An invertable R object.
#' @param ... further arguments passed to \code{\link{mean}}
#' @export
#' @seealso \code{\link{geometricMean}} and \code{\link{mean}}
`harmonicMean` <- function(x, ...)
{
  return(1/(mean(1/x, ...)))
}
NULL



#' Calculate a weighted geometric mean
#'
#' @param x An R object.
#' @param ... further arguments passed to \code{\link{prod}}
#' @export
#' @seealso \code{\link{weightedHarmonicMean}}, \code{\link{weighted.mean}}, \code{\link{geometricMean}}, \code{\link{harmonicMean}} and \code{\link{mean}}
weightedGeometricMean <- function(x, w, ...)
{
  return(prod(x^w, ...)^(1/sum(w)))
}
NULL



#' Calculate a weighted harmonic mean
#'
#' @param x An invertable R object.
#' @param ... further arguments passed to \code{\link{sum}}
#' @export
#' @seealso \code{\link{weightedGeometricMean}}, \code{\link{weighted.mean}}, \code{\link{harmonicMean}} \code{\link{geometricMean}} and \code{\link{mean}}
weightedHarmonicMean<- function(x, w, ...)
{
  return(sum(w)/(sum(w/x, ...)))
}
NULL

