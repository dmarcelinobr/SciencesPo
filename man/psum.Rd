\encoding{UTF-8}
\name{psum}
\alias{psum}
\title{Parallel sum
}
\description{Provides parallel sum like \code{pmin} and \code{pmax} from the base package. The function \code{sum} simply does not help when the objective is to obtain a vector with parallel sum rather than a scalar value.
}
\usage{
psum(..., na.rm = FALSE)
}
\arguments{
  \item{\dots}{One or more unit objects.
}
  \item{na.rm}{A logical value TRUE or FALSE, the default.
}
}

\value{A vector containing the parallel sum.
}
\author{Daniel Marcelino <dmarcelino@live.com>
}


\examples{
	data(us2012)
	psum(us2012$Obama, us2012$Romney)
	Swingy <-psum(us2012$Obama, us2012$Romney-100)
}
\keyword{ descriptive stats }
\keyword{ attributes }
