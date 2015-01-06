#' Calculate the Log Likelihood of a Normal Distribution
#' 
#' @description 
#' Find the log likelihood of a normal distribution.
#' 
#' @param x data.
#' @param mu estimated mean.
#' @param var estimated variance.
#' 
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' 
#' @return ll logliklihood of the distribution
#' 
#' @examples
#' x = rnorm(100, 3, 7)
#' logLik(x,3,7)
#' 
#' @export 

logLik<-function(x=data, mu, var)
{
  n=length(x)
  ll = -n/2* log(2*pi*var) - .5/var*sum((mu-x)^2)
  
  -ll
}
