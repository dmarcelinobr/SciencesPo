\encoding{UTF-8}
\name{soundexES}
\alias{soundexES}
\title{Spanish Soundex Encoding
}
\description{soundexES returns a census-like soundex code a string name, given the Spanish sound system. This function was firstly outlined to work with \code{RecordLinkage} package, however, it also is helpful as a standalone function. See details bellow.
}
\usage{
soundexES(term)
}
\arguments{
  \item{term}{is the vector with character strings.
}
}
\value{ A character vector or matrix with the same dimensions as \code{term}.
}
\references{
Borg, Andreas and Murat Sariyar. (2012) \emph{RecordLinkage: Record Linkage in R,} R package version 0.4-1,  \url{http://CRAN.R-project.org/package=RecordLinkage}.

}
\author{Daniel Marcelino <dmarcelino@live.com>
}
\note{This function was adapted from the US census soundex version. See in \url{http://archives.gov/research/census/soundex.html}
}

\seealso{\code{\link{soundexBR}}, \code{\link{soundexFR}} .
}

\examples{

soundexES(x <- c('Alejandro Angel Tapias', 'Alejandro Tápias',
'Alejandro A. Tapias', 'Alexander Angel Tápias',
'Alexandre Angel Tapias'))

}

\keyword{ soundex  }
