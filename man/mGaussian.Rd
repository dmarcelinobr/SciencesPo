\encoding{UTF-8}
\name{mGaussian}
\alias{mGaussian}
\title{Maximize Gaussian

}
\description{This function is used for Maximizing Gaussian
}
\usage{
mGaussian(mu, sigma2, x)
}
\arguments{
  \item{mu}{ is the mean }
    \item{sigma2}{ is the variance }
  \item{x}{ is the state we know nothing about it.}
}

\value{ The value that maximize Gaussian
}
\references{Udacity course in Kalman Filter
}
\author{Daniel Marcelino 
}
\note{We can set x = mu, so that we get the peak of the Gaussian  as the peak always occurs at the mean (looking at a Gaussian distribution). This is why we set x= when we want to maximize.}

\examples{
mGaussian(10, 4, 10)
}
\keyword{ Gaussian }