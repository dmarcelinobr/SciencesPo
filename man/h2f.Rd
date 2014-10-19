\encoding{UTF-8}
\name{h2f}
\alias{h2f}
\title{
Shows the head and the foot lines of a data frame
}
\description{
Shows the first and last lines of an object simultaneously .
}
\usage{
h2f(x, head = 4, tail = 4, digits = 2, ellipsis = TRUE)
}
\arguments{
  \item{x}{
  	An object.

}
  \item{head}{
	A numeric value for lines to show.
}
  \item{tail}{
A numeric value for lines to show.
}
  \item{digits}{
  	A numeric value for digits to print.
}
  \item{ellipsis}{
  A logical value. If TRUE, ellipsis will be printed between the first and last lines.
}
}

\value{
An object of the same type.
}
\author{
Daniel Marcelino <dmarcelino@live.com>
}

\seealso{
\code{\link{detail}}
}
\examples{
data(galton)
h2f(galton)
}
\keyword{ tables }
