\encoding{UTF-8}
\name{rDirichlet}
\alias{rDirichlet}
\title{Sampling from Dirichlet distribution.}
\usage{
  rDirichlet(n, alpha)
}
\arguments{
  \item{n}{Number of random vectors to generate.}

  \item{alpha}{Vector containing shape parameters.}
}
\value{
  returns a matrix with n rows, each containing a single
  Dirichlet random deviate.
}
\description{
  Generate random deviates from the Dirichlet distribution.
}
\examples{
# 1 - Simple usage
rDirichlet(20, c(1,1,1) )
# 2 - 
alpha = c( 5.0, 1.0, 2.0 )

alpha.0 = sum( alpha )

test = rDirichlet( 100000, alpha )

apply( test, 2, mean )

alpha / alpha.0

apply( test, 2, var )

alpha * ( alpha.0 - alpha ) / ( alpha.0^2 * ( alpha.0 + 1 ) )

# 3 - Brazil poll, by Datafolha 
### Face-to-face interviews conducted on Oct 03-04 with n = 18116
n <- 18116
poll <- c(40,24,22,5,5,4) / 100 * n # data

### draw a sample from the posterior
set.seed(1234)
mcmc <- 100000
sim <- rDirichlet(mcmc, alpha = poll + 1)
### look at the margins of Aecio over Marina in the very last minute of the campaign:
margin <- sim[,2] - sim[,3]
mn <- mean(margin) # Bayes estimate
mn
s <- sd(margin) # posterior standard deviation

qnts <- quantile(margin, probs = c(0.025, 0.975)) # 90% credible interval
qnts
pr <- mean(margin > 0) # posterior probability of a positive margin
pr
## plot posterior density
hist(margin, prob = TRUE, # posterior distribution
  breaks = "FD", xlab = expression(p[2] - p[3]),
main = expression(paste(bold("Posterior distribution of "), p[2] - p[3])))
abline(v=mn, col='red', lwd=3, lty=3)

}

\author{
  Code original posted by Ben Bolker to R-News on Fri Dec
  15 2000. See
  \url{http://www.r-project.org/nocvs/mail/r-help/2000/3865.html}.
  Ben attributed the code to Ian Wilson <i.wilson
}

\keyword{ distributions }