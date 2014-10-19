\encoding{UTF-8}
\name{rescale}
\alias{rescale}
\title{Rescaling as Z-scores and Centering 
}
\description{Provides a rapidly way to generate standard variables. 
}
\usage{
rescale(x, FUN = "center")
}
\arguments{
  \item{x}{ A vector to be computed}
  \item{FUN}{A function to be applied: `z-score' or `z', the default is `center' or `c'. }
 
}

\value{An object the same class as \code{x}.}

\author{Daniel Marcelino <dmarcelino@live.com>
	}
\examples{
x <- sample(10)
rescale(x) # centering 
rescale(x, FUN=="full")
rescale(x, FUN=="-0.5,0.5") # for binary input
rescale(x, FUN=="0/1") # for binary input
}
\keyword{ models }
\keyword{standardization }

