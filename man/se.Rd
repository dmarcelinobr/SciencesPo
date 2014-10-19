\encoding{UTF-8}
\name{se}
\alias{se}
\alias{std.error}
\title{Standard Error
}
\description{Computes the standard errors for a numeric vector.
}
\usage{
se(x, na.rm = TRUE)}

\arguments{
  \item{x}{ A vector of class numeric or integer.
}
  \item{na.rm}{A logical value indicating whether NA values should be stripped before the computations. }
}

\details{ The standard error of the mean (SEM) (\emph{assuming statistical independence of the values in the sample}) is estimated by taking the standard deviation of the population sample, divided by the square root of the sample size: \deqn{se = \frac{{s}}{{\sqrt{n}}}}
}


\value{ An object of the same type as \code{x}.
}

\references{
Agresti, A., and Finlay, B. (1997). \emph{Statistical methods for the social sciences.}

Kenney, J. F. and Keeping, E. S. (1951) Standard Error of the Mean. In \emph{Mathematics of Statistics,} Pt. 2, 2nd ed. Princeton, NJ: Van Nostrand, pp. 110 and 132--133.

}
\author{Daniel Marcelino <dmarcelino@live.com>
}

\seealso{ \code{\link{detail}}.
}
\examples{
x <- rnorm(100)

se(x)

detail(x)

}

\keyword{ descriptive stats }
