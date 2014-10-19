\encoding{UTF-8}
\name{us2012}
\alias{us2012}
\docType{data}
\title{US Presidential Election Opinion Polls
}
\description{Opinion Polls for the 2012 presidential campaign.
}
\usage{data(us2012)}
\format{
  A data frame with 588 observations and 10 variables.
  \describe{
    \item{\code{Pollster}}{a factor with pollster firm names.}
    \item{\code{Start}}{starting date of the field work.}
    \item{\code{End}}{ending date of the field work.}
    \item{\code{Released}}{date that the results were released.}
    \item{\code{N}}{sample size.}
    \item{\code{Population}}{population interviewed.}
	\item{\code{Mode}}{type of interview.}
	\item{\code{Obama}}{proportion support for Obama.}
	\item{\code{Romney}}{proportion support for Romney.}
	\item{\code{Undecided}}{undecided proportion.}
  }
}

\source{Several sources, but see details in the data file itself.}

\examples{
data(us2012)
}
\keyword{datasets}
