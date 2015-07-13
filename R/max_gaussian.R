#' @title Maximize Gaussian
#'
#' @description This function Maximizes a Gaussian Distribution
#' @param mu The mean component
#' @param sigma2 The variance component 
#' @param x Is the state we know nothing about.
#'
#'  @details We can set x = mu, so that we get the peak of the Gaussian  as the peak always occurs at the mean (looking at a Gaussian distribution). This is why we set x= when we want to maximize.
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' 
#' @return  The value that maximize Gaussian
#' @references Udacity course on Kalman Filter
#' @examples 
#' maxGaussian(10, 4, 10)
#' @export
maxGaussian <- function(mu, sigma2, x){
  return(1/sqrt(2.*pi*sigma2)*exp(-.5 * (x-mu)**2 / sigma2) )
}