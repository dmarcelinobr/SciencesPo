\encoding{UTF-8}
\name{get.ci}
\alias{get.ci}

\title{Confidence interval of a vector.

}
\description{Calculates the confidence interval of a vector of data.
}
\usage{
get.ci(vector, ci = 0.95)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vector}{A vector of numeric values.
}
  \item{ci}{
The confidence interval to be calculated.
}
}
\value{
\item{upper }{Upper bound of interval.}
\item{mean }{Mean of data.}
 \item{lower}{Lower bound of interval.}
}
\author{
Daniel Marcelino <dmarcelino@live.com>
}
\examples{
	myvec <-rnorm(100)
get.ci(myvec)
get.ci(myvec, ci=.99)
}

\keyword{ descriptive stats }

