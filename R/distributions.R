#' @title Inverse Cumulative Standard Normal Distribution 
#' 
#' @description Computes the inverse cumulative distribution of \code{x} associated with an \emph{area} under the normal distribution curve given by \eqn{\mu} and standard deviation  \eqn{\sigma}. 
#' 
#' @param area the area or a vector of probabilities.
#' @param mu the mean \eqn{\mu}.
#' @param sigma the standard deviation of the distribution \eqn{\sigma}.
#' 
#' @seealso \code{\link{normalpdf}}, \code{\link{normalcdf}}
#' 
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' 
#' @keywords distribution
#'  @examples
#' invNormal(area=0.35,mu=0,sigma=1)
#' 
#' @export
"invNormal" <-
  function(area,mu=0,sigma=1){
    qnorm(p=area,mean=mu,sd=sigma)
  }
NULL

#' @title Normal probability density function 
#' 
#' @description Computes the pdf at each of the values in \emph{x} using the normal distribution with mean \eqn{\mu = 0} and standard deviation \eqn{\sigma = 1}.
#'
#' @param x a vector of quantiles.
#' @param mu is the mean \eqn{\mu}, its default value is \eqn{\mu = 0}
#' @param sigma is the standard deviation \eqn{\sigma}, its default value is \eqn{\sigma = 1}
#' 
#' @note The pdf function is given by:  \deqn{f(x) = \frac{1}{\sqrt{2 \pi} \sigma} \exp\left(\frac{- (x - \mu)^2}{2 \sigma^2}\right)}{f(x) = 1/(sqrt(2 \pi) \sigma) e^-((x - \mu)^2/(2 \sigma^2))} 
#' for \eqn{\sigma > 0}

#' @keywords distribution
#' 
#' @seealso \code{\link{normalcdf}}, \code{\link{invNormal}}.
#' #' 
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @examples
#'   normalpdf(x=1.2,mu=0,sigma=1)
#'
#' @export
"normalpdf" <-
  function(x, mu=0,sigma=1){
    dnorm(x, mean=mu,sd=sigma)
  }
NULL


 
#' @title Normal Cumulative Distribution 
#' 
#' @description Calculates the normal distribution probability using \emph{lower bound} e \emph{upper bound} by the mean \eqn{\mu} and standard deviation.

#' @param lower is the inferior extreme value. 
#' @param upper is the superior extreme value.
#' @param mu is the mean \eqn{\mu}, its default value is \eqn{\mu = 0}
#' @param sigma is the standard deviation \eqn{\sigma}, its default value is \eqn{\sigma = 1}
#' 
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' 
#' @seealso \code{\link{normalpdf}}, \code{\link{invNormal}}.
#' 
#' @keywords distribution
#' 
#' @examples
#' normalcdf(lower=-1.96,upper=1.96,mu=0,sigma=1)
#' @export
"normalcdf" <-
  function(lower,upper,mu=0,sigma=1){
    abs(pnorm(upper,mean=mu,sd=sigma)-pnorm(lower,mean=mu,sd=sigma))
  }
NULL




#' @title Samples from Dirichlet distribution
#' 
#' @description Generates random deviates from the Dirichlet distribution. This code was originally posted by Ben Bolker to R-News on Fri Dec 15 2000. See \url{http://www.r-project.org/nocvs/mail/r-help/2000/3865.html}. But Ben Bolker attributed the code to Ian Wilson.
#' 
#' @param n Number of random vectors to generate
#' @param alpha Vector containing shape parameters
#' 
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' 
#' @return Returns a matrix with n rows, each containing a single Dirichlet random deviate.

#' @examples
#' 
#' # 1 - Simple usage
#' rDirichlet(20, c(1,1,1) )
#' # 2 - 
#' alpha = c( 5.0, 1.0, 2.0 )
#' alpha.0 = sum( alpha )
#' test = rDirichlet( 100000, alpha )
#' apply( test, 2, mean )
#' alpha / alpha.0
#' apply( test, 2, var )
#' alpha * ( alpha.0 - alpha ) / ( alpha.0^2 * ( alpha.0 + 1 ) )
#' # 3 - Brazil poll, by Datafolha 
#' ## Face-to-face interviews conducted on Oct 03-04 with n = 18116
#' n <- 18116
#' poll <- c(40,24,22,5,5,4) / 100 * n # data

### draw a sample from the posterior
#' set.seed(1234)
#' mcmc <- 100000
#' sim <- rDirichlet(mcmc, alpha = poll + 1)
### look at the margins of Aecio over Marina in the very last minute of the campaign:
#' margin <- sim[,2] - sim[,3]
#' mn <- mean(margin) # Bayes estimate
#' mn
#' s <- sd(margin) # posterior standard deviation
#'
#' qnts <- quantile(margin, probs = c(0.025, 0.975)) # 90% credible interval
#' qnts
#' pr <- mean(margin > 0) # posterior probability of a positive margin
#' pr
## plot posterior density
#' hist(margin, prob = TRUE, # posterior distribution
#'     breaks = "FD", xlab = expression(p[2] - p[3]),
#'     main = expression(paste(bold("Posterior distribution of "), p[2] - p[3])))
#' abline(v=mn, col='red', lwd=3, lty=3)
#' 
#' 
#' @keywords Distributions
#' 
#' @export
rDirichlet <-
  function( n, alpha ) {
    
    l = length( alpha )
    
    theta = matrix( 0, n, l )
    
    for ( j in 1:l ) {
      
      theta[ , j ] = rgamma( n, alpha[ j ], 1 )
      
    }
    
    theta = theta / apply( theta, 1, sum )
    
    return( theta )
    
  }
NULL


#' @title Binomial cumulative distribution function
#' 
#' @description Computes a binomial cdf at each of the values in \code{x} using the corresponding number of trials in \code{n} and probability of success for each trial in \code{p}.  
#' 
#' @param n  the number of trials.
#' @param p a vector of probabilities.
#' @param x the number of success.
#' 
#' @examples
#' trials = 10
#' prob = c(.2,.25,.3,.35)
#' success = 4
#' binompdf(n = trials, p = prob, x = success)
#' @export
"binomcdf" <-
  function(n,p,x){
    pbinom(x,size=n,prob=p)
  }
NULL





#' @title Binomial probability density function
#' 
#' @description Computes the binomial pdf at each of the values in \code{x} using the corresponding number of trials in \code{n} and probability of success for each trial in \code{p}. 
#' 
#' @param n  the number of trials.
#' @param p a vector of probabilities.
#' @param x the number of success.
#' 
#' @note The probability density function (pdf) is given by: \deqn{p(x) = {n \choose k} p^x (1 - p)^{n - x}}{p(x) = choose(n,x) p^x (1-p)^(n-x)} with \eqn{x = 0, 1, 2, \ldots}

#' @examples
#' trials = 10
#' prob = c(.2,.25,.3,.35)
#' success = 4
#' binomcdf(n = trials, p = prob, x = success)
#' 
#' @export
"binompdf" <-
  function(n,p,x){
    dbinom(x,size=n,prob=p)
  }
NULL


