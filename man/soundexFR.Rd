\encoding{UTF-8}
\name{soundexFR}
\alias{soundexFR}
\title{French Soundex Encoding
}
\description{soundexFR returns a census-like soundex code a string name, given the French sound system. This function was firstly outlined to work with \code{RecordLinkage} package, however, it also is helpful as a standalone function. See details bellow.
}
\usage{
soundexFR(term)
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
\note{This function is an adaptation of the US census soundex version. See in \url{http://archives.gov/research/census/soundex.html}
}

\seealso{\code{\link{soundexBR}},\code{\link{soundexES}}.
}

\examples{

soundexFR(x <- c('Jean Charest', 'John Charest',
	'James Charest', 'John James Charest'))
}
\keyword{ soundex  }
