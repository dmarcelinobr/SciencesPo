\encoding{UTF-8}
\name{outliers}
\alias{outliers}
\title{Detection of Outliers
}
\description{Perform an exploaratory test to detect \emph{outliers}. This function returns the minimum and maximum values, respectively preceded by their positions in the \code{vector}, \code{matrix} or \code{data.frame}. The quantity for \emph{min} reveals the minimum deviation from the mean, the integer in \emph{closest} highlights the position of the element. In the same vein, the quantity for \emph{max} is the maximum deviation from the mean, and the \code{farthest} integer is the position of such higher quantity.
}
\usage{
outliers(x, index = NULL)
}
\arguments{
  \item{x}{A numeric object.
}
  \item{index}{ A numeric value to be considered in the computations.
}
}

\value{ The returning object will depend on the inputing object, either a vector or a data frame.

}
\references{Dixon, W.J. (1950) Analysis of extreme values. \emph{Ann. Math. Stat.} \bold{21(4),} 488--506.
}
\author{Daniel Marcelino <dmarcelino@live.com>
}

\seealso{For diminish the impact of outliers see \link{winsor.mean}.
}
\examples{
#data frame;
data(ltaylor96)

outliers(ltaylor96)


## vetor
outliers(ltaylor96$spot)

outliers(x <- rnorm(20))

}

\keyword{ attributes }
\keyword{ descriptive stats }
