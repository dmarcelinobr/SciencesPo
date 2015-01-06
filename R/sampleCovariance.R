#' @encoding UTF-8
#' @title Calculate the Sample Covariance
#' 
#' @description Computes the sample covariance between two vectors. The Covariance provides a measure of the strength of the correlation between two or more sets of random variates. The covariance for two random variates \code{x} and \code{y}, each with sample size \code{n}, is defined by the expectation value variables \verb{cov(x, y) = (x - \mu_x)(y - \mu_y)}. For uncorrelated variables, \code{cov(x, y) = 0}. 
#' 
#' @param x One of two vectors whose sample covariance is to be calculated.
#' @param y The other vector.
#' @param verbose If \code{TRUE}, prints sample covariance; if not, not. Default is \code{verbose = TRUE}.
#' 
#' @return The sample covariance between x and y.
#' 
#' @details x and y must have the same length, greater than one  with no missing values.
#' 
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @references Based on the Google's R Guide Style.
#' @examples
#' # Some random data:
#' df = data.frame(id=1:20, x=rnorm(20, mean=2, sd=.5), 
#'  y=rnorm(20, mean=5, sd=2) )
#'   sampleCovariance(df$x, df$y)
#'
#' @export
sampleCovariance <- function(x, y, verbose = TRUE) {
  n <- length(x)
  # Error handling
  if (n <= 1 || n != length(y)) {
    stop("Arguments x and y have different lengths: ",
         length(x), " and ", length(y), ".")
  }
  if (TRUE %in% is.na(x) || TRUE %in% is.na(y)) {
    stop(" Arguments x and y must not have missing values.")
  }
  covariance <- var(x, y)
  if (verbose)
    cat("Covariance = ", round(covariance, 4), ".\n", sep = "")
  return(covariance)
}