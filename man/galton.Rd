\name{galton}
\alias{galton}
\docType{data}
\title{Galton's Family Data on Human Stature.
}
\description{It is a reproduction of the data set used by Galton in his 1885's paper on correlation between parent's height and their children. However, Galton would only introduce the concept of correlation few years later, in 1888. Galton suggested the use of the regression line and was the first to describe the so-called common phenomenon of regression toward the mean by comparing his experiments on the size of the seeds of successive generations of peas. 
}
\usage{data(galton)}
\format{
  A data frame with 928 observations on the following 2 variables.
  \describe{
    \item{\code{parent}}{The parents' average height}
    \item{\code{child}}{The child's height}
  }
}
\details{Regression analysis is the statistical method most often used in
political science research. The reason is that most scholars are
interested in identifying ``causal'' effects from non-experimental
data and that regression is the method for doing this.
The term ``regresssion'' (1889) was first crafted by Sir Francis Galton upon investigating the relationship between body size of fathers and sons. Thereby he
``invented'' regression analysis by estimating: \eqn{S_s = 85.7 + 0.56S_F}
meaning that the size of the son regresses towards the
mean.
}

\references{ Francis Galton (1886) Regression Towards Mediocrity in Hereditary Stature. \emph{The Journal of the Anthropological Institute of Great Britain and Ireland,} Vol. \bold{15}, pp. 246--263. 

James A. Hanley (2004) \sQuote{Transmuting} women into men: Galton's family data on human stature. \emph{The American Statistician,} \bold{58(3),} 237-243.
}
\examples{
data(galton)

}
\keyword{datasets}
