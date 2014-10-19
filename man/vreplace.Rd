\encoding{UTF-8}
\name{vreplace}
\alias{vreplace}

\title{Replace elements

}
\description{Replace many elements in a vector at the same time
}
\usage{
vreplace(vector, from, to)
}

\arguments{
  \item{vector}{ A vector
}
  \item{from}{
 From what}
  \item{to}{
To what}
}

\author{
Daniel Marcelino}

\examples{

countries <- c('United States', 'Ecuador', 'Russia', 'Russia', 'Ecuador')
vreplace(vec=countries, from=c('Ecuador', 'Russia', 'United States'), to=c('ECU', 'RUS', 'USA'))
}
\keyword{ data manipulation }
