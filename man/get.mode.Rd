\encoding{UTF-8}
\name{get.mode}
\alias{get.mode}
\title{Estimates mode
}
\description{Estimates the mode in a vector

}
\usage{
get.mode(x, na.rm = FALSE)
}
\arguments{
  \item{x}{ A data vector. }
\item{na.rm}{A logical value, default is FALSE. }
}

\author{Daniel Marcelino
}

\examples{
vec <-c(1,1,2,2,3,3,4,4,5, NA)
get.mode(vec, na.rm = TRUE)
}

\keyword{ descriptive stats }
