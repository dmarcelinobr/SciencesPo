#' @encoding UTF-8
#' @title Pearson's Coefficient of Variation
#'
#' @description Compute the absolute \bold{coefficient of variation} \bold{cv} as proposed by Karl Pearson, which is given by the division of standard deviation by the mean. The CV reflects a normalized measure of the dispersion of a given probability distribution. Conversely, distributions with \eqn{cv < 1} are considered \dQuote{low-variance}, while those with \eqn{cv > 1} \dQuote{high-variance}.
#'
#' @param x A numeric vector.
#' @param na.rm A logical value, default is \code{FALSE}
#' @details \eqn{\frac{sd(x)}{mean(x)} = cv}, which is the inverse of signal-to-noise ratio.
#'
#' @return The coefficient of variation.
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#'
#' @seealso \code{\link{se}}, \code{\link{skewness}}, \code{\link{kurtosis}}, \code{\link{winsorize}}, \code{\link{outliers}}
#'
#' @keywords Descriptive Stats
#' @examples
#' set.seed(51);
#' x <- sample(100);
#' cv(x);
#'
#' @export cv
#' @docType methods
#' @rdname cv-methods
#' @aliases cv,numeric,logical,ANY-method
`cv`<-setClass("cv", representation(x = "numeric", na.rm="logical"))
setGeneric("cv", def=function(x, ...){
  standardGeneric("cv")
})

#' @rdname cv-methods
setMethod(f="cv", definition=function(x, na.rm = TRUE){
  sd <- sd(x, na.rm = na.rm)
  mean <- mean(x, na.rm = na.rm)
  return(sd/mean)
})
NULL
