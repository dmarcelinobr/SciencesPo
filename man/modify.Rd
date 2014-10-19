\encoding{UTF-8}
\name{modify}
\alias{modify}
\title{Modify data elements
}
\description{Modify an element of a vector
}
\usage{
modify(x, position, value)
}
\arguments{
  \item{x}{
data object.
}
  \item{position}{
Position of the element to be replaced}
  \item{value}{
Value to modify }
}
\author{
Daniel Marcelino
}
\examples{
x <- seq(1:10)
modify(x, 1, 10)
}

\keyword{ data manipulation }

