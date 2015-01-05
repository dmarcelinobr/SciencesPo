#' @title Pearson's Coefficient of Variation 
#'
#' @description Compute the absolute \bold{coefficient of variation} \bold{cv} as proposed by Karl Pearson, which is given by the division of standard deviation by the mean. The CV reflects a normalized measure of the dispersion of a given probability distribution. Conversely, distributions with \deqn{cv < 1} are considered \dQuote{low-variance}, while those with \deqn{cv > 1} \dQuote{high-variance}.
#' 
#' @param x A numeric vector.
#'
#' @details \code{sd(x)/mean(x) = cv}, which is the inverse of signal-to-noise ratio.
#'
#' @return The coefficient of variation.
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#'
#' @keywords The Basics
#' @keywords Descriptive Stats
#'
#' @examples
#'
#' myvar <- sample(100) 
#' variation(myvar)
#'
#' @export
#' 
variation <-  
function(x){ 
  sd(x)/mean(x)
}
