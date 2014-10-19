\name{bhodrick93}
\alias{bhodrick93}
\docType{data}
\title{Bekaert's and Hodrick's (1993) Data
}
\description{Data set used by Bekaert and Hodrick (1993) on biases in the measurement of foreign exchange risk premiums.
}
\usage{data(bhodrick93)}
\format{
  A data frame with 778 observations on the following 11 variables. See Hayashi (2000) for details.
  \describe{
    \item{\code{date}}{A character vector for date.}
    \item{\code{jyspot}}{Price of USD in JY, spot.}
    \item{\code{jyfwd}}{Price of USD in JY, 30-day forward.}
    \item{\code{jys30}}{Price of USD in JY, spot market at 30-day forward deliver/date.}
    \item{\code{dmspot}}{Price of USD in DM, spot.}
    \item{\code{dmfwd}}{Price of USD in DM, 30-day forward}
    \item{\code{dms30}}{Price of USD in DM, spot market at 30-day forward deliver/date.}
    \item{\code{bpspot}}{Price of USD in BP, spot.}
    \item{\code{bpfwd}}{Price of USD in BP, 30-day forward.}
    \item{\code{bps30}}{Price of USD in BP, spot market at 30-day forward deliver/date.}
    \item{\code{quote}}{A numeric vector.} 
	}
}
\source{ Hayashi, F. (2000) \emph{Econometrics.} Princeton. New Jersey, USA: Princeton University.

\url{http://fmwww.bc.edu/ec-p/data/Hayashi/}
}
\references{Bekaert, G., and Hodrick, R. J. (1993) On biases in the measurement of foreign exchange risk premiums. \emph{Journal of International Money and Finance,} \bold{12(2),} 115-138.
}
\examples{
data(bhodrick93)
}
\keyword{datasets}
