\name{slice}
\alias{slice}
\title{Slice a vector
}
\description{Break up a vector by certain N sized chunks}
\usage{
slice(input, by = 2, pattern = NULL)
}
\arguments{
  \item{input}{ Input character vector
}
  \item{by}{Number by which to split the input character vector.}
  \item{pattern}{A value.
}
}

\author{Daniel Marcelino	
}

\examples{
x <- seq(1:15)
slice(x, by = 2, pattern = 3)

}

\keyword{ data manipulation }
