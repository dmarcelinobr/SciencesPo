\encoding{UTF-8}
\name{get.years}
\alias{get.years}
\title{Get date difference in years.
}
\description{
Returns difference in years between two date points.}
\usage{
get.years(earlier, later)
}
\arguments{
  \item{earlier}{
  	An earlier date variable. 
}
  \item{later}{
An later date variable. }
}

\value{An integer vector for time in years.

}
\references{Daniel Marcelino and Alejandro Tapias (2014)  When the Winner Comes Third.  \emph{IJAS,} \bold{30(6),} 1640--1648.
}
\author{Daniel Marcelino <dmarcelino@live.com>
}
\note{
Variables must as \code{as.Date}}

\seealso{ \code{\link{shift}}.
}
\examples{
earlier <- as.Date("2012-11-05")
later <- as.Date("2014-11-06")
get.years(earlier, later)
}

\keyword{ descriptive stats }
\keyword{ attributes }
