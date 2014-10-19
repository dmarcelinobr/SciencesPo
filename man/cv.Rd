\encoding{UTF-8}
\name{cv}
\alias{cv}
\title{Computes Pearson's coefficient of variation 
}
\description{Computes the absolute coefficient of variation as
  proposed by Pearson: the standard deviation divided by the
  mean. Essentially, cv reflects a normalized measure of dispersion of a probability distribution. Distributions with cv < 1 are considered low-variance, while those with cv > 1 high-variance.
}
\usage{
cv(x)
}
\arguments{
  \item{x}{ is a numeric vector.
}
}
\details{ sd(x)/mean(x) = cv, which is the inverse of signal-to-noise ratio.
}
\value{Coefficient of variation value.
}

\author{Daniel Marcelino <dmarcelino@live.com> 
}
\examples{
v <- rnorm(10) 
cv(v)
}

\keyword{ descriptive stats }
