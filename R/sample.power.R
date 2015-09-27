#' @title Calculates and plots power of one sample
#'
#' @description Calculates and plots power of one sample z-test of a sample mean mu1
#'  against a population mean \code{mu0} (H_{0}: mu0 = mu1, H_{1}: mu0 <> mu1).
#'
#' @param mu0 This should be the "known" mean value for your population.
#' @param mu1 This should be the "expected" mean value from your sample. The delta between mu(0) and mu(1) is what you should consider a significant difference for the test.
#' @param n The sample size.
#' @param sigma This should be the known sigma (standard deviation) for the population.
#' @param alpha  This is the significance level, default is alpha(twosided) = .05.
#'
#' @details
#' \code{sample.power} calculates the power of a one-sample z-test (twosided)
#' and plots the density distributions under the assumption of of H_{0}: m = mu0 and
#' H_{1}: m = mu1. The rejection regions of H_{0} (alpha) are colored blue, while the rejection region of H_{1} (beta) is colored red.
#'
#' @return
#' \code{n} the sample size;
#' \code{sigma} the standard deviation;
#' \code{SE} the standard error of the mean;
#' \code{mu0} the mean of H_{0} in the population;
#' \code{mu1} the sample mean;
#' \code{mean.crit} the critical value of sample mean to achieve significance;
#' \code{ES} the population "effect" size gamma;
#' \code{delta} the effect size delta (Cohen);
#' \code{alpha} the significance level alpha (twosided);
#' \code{power} the power (1-beta).
#'
#' @examples
#' sample.power(mu0=68, mu1=69, sigma=3.1, n=100)
#' ## gives a power of .90
#'
#' @export
`sample.power` <- function(mu0=0, mu1=0, sigma=1, n=100, alpha=.05) {
  gamma = (mu1-mu0)/sigma
  delta = gamma*sqrt(n)
  se.mean = sigma/sqrt(n)

  z0L = mu0-stats::qnorm(1-alpha/2)*se.mean
  z0U = mu0+stats::qnorm(1-alpha/2)*se.mean
  z0min = mu0-3.5*se.mean
  z0max = mu0+3.5*se.mean
  z1min = mu1-3.5*se.mean
  z1max = mu1+3.5*se.mean
  if (mu1 > mu0) mean.crit=z0U
  else mean.crit=z0L

  if (mu1 != mu0) {
    cat('\nOne-sample z-test power calculation\n\n')
    cat('          n =',n,'\n')
    cat('          \u03C3 =',round(sigma,3),'\n')
    cat('   SE(mean) =',round(se.mean,3),'\n')
    cat('         \u03BC0 =',round(mu0,3),'\n')
    cat('         \u03BC1 =',round(mu1,3),'\n')
    cat('  mean.crit =',round(mean.crit,3),'\n\n')
    cat('effect size =',round(gamma,3),'\n')
    cat('      delta =',round(delta,3),'\n')
    cat('  sig.level =',round(alpha,3),'\n')
    cat('      power =',round(1-stats::pnorm(stats::qnorm(1-alpha/2,mean=0,sd=se.mean),
                                      mean=abs(mu1-mu0),sd=se.mean),3),'\n')
    cat('alternative = two-sided\n\n')
    if (n < 30) cat('Warning: N too small for z test!\n\n')
  }

  # Normal curve H0:

  curve(stats::dnorm(x,mean=mu0,sd=se.mean),from=z0min,to=z0max,
        xlim=range(z0min,z0max,z1min,z1max),ylab='density',col="blue",
        xlab=paste('\u03B1 = ',round(alpha,3),' (two-sided)',
                   ', \u03B2 = ',round(stats::pnorm(stats::qnorm(1-alpha/2,mean=0,
                                                 sd=se.mean),mean=abs(mu1-mu0),
                                           sd=se.mean),3),
                   ', power = ',round(1-stats::pnorm(stats::qnorm(1-alpha/2,mean=0,
                                                    sd=se.mean),mean=abs(mu1-mu0),
                                              sd=se.mean),3),sep=""),
        main=paste('Power of z-Test of the Mean of a Single Population\n',
                   'n = ',n,', \u03BC0 = ',mu0,', \u03BC1 = ',mu1,', \u03C3 = ',sigma, sep=""))
  # Acceptance region H0:
  x=seq(z0L,z0U,min(.001,1/n))
  graphics::polygon(c(z0L,x,z0U),c(0,stats::dnorm(x,mean=mu0,sd=se.mean),0),col="lightyellow")

  # Normal curve H1:
  graphics::curve(dnorm(x,mean=mu1,sd=se.mean),from=z1min,to=z1max,add=TRUE,col="red")

  text(mu0,0, '\u03BC0',pos=1,offset=.15,cex=.8)
  text(mu1,0, '\u03BC1',pos=1,offset=.15,cex=.8)

  if (mu1 > mu0){
    text(z0U,0,round(z0U,2),pos=1,offset=.15,cex=.8)

    # Acceptance region H1:
    x=seq(z0U,z1max,min(.001,1/n))
    graphics::polygon(c(z0U,x,z1max),c(0,stats::dnorm(x,mean=mu1,sd=se.mean),0),
            col="lightyellow")

    # Rejection region H0:
    x=seq(z0U,z0max,min(.001,1/n))
    graphics::polygon(c(z0U,x,z0max),c(0,stats::dnorm(x,mean=mu0,sd=se.mean),0),density=20,
            col="blue")
    x=seq(z0min,z0L,min(.001,1/n))
    graphics::polygon(c(z0min,x,z0L),c(0,stats::dnorm(x,mean=mu0,sd=se.mean),0),density=20,
            col="blue")

    # Rejection region H1:
    if (z1min < z0U) {
      x=seq(z1min,z0U,min(.001,1/n))
      graphics::polygon(c(z1min,x,z0U),c(0,stats::dnorm(x,mean=mu1,sd=se.mean),0),density=20,
              angle=135,col="red")
    }
  }
  else if (mu1 < mu0) {
    text(z0L,0,round(z0L,2),pos=1,offset=.15,cex=.8)

    # Acceptance region H1:
    x=seq(z1min,z0L,min(.001,1/n))
    graphics::polygon(c(z1min,x,z0L),c(0,stats::dnorm(x,mean=mu1,sd=se.mean),0),
            col="lightyellow")

    # Rejection region H0:
    x=seq(z0U,z0max,min(.001,1/n))
    graphics::polygon(c(z0U,x,z0max),c(0,stats::dnorm(x,mean=mu0,sd=se.mean),0),density=20,
            col="blue")
    x=seq(z0min,z0L,min(.001,1/n))
    graphics::polygon(c(z0min,x,z0L),c(0,stats::dnorm(x,mean=mu0,sd=se.mean),0),density=20,
            col="blue")

    # Rejection region H1:
    if (z0L < z1max) {
      x=seq(z0L,z1max,min(.001,1/n))
      graphics::polygon(c(z0L,x,z1max),c(0,stats::dnorm(x,mean=mu1,sd=se.mean),0),density=20,
              angle=135,col="red")
    }
  }
  else cat('Error: \u03BC1 must differ from \u03BC0!\n')
}
NULL
