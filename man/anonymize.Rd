\encoding{UTF-8}
\name{anonymize}
\alias{anonymize}
\title{Make Anonymous Data
}
\description{It replaces factor and character variables by a combination of letters and numbers, the numeric columns are also transformed.
}
\usage{
anonymize(x, keep.names=TRUE)
}
\arguments{
  \item{x}{A vector or a data frame.}
  \item{keep.names}{A logical argument. If false, variable names will be replaced by Xs.}
}

\details{By making difficult to recognize the original data while keeping the same data structure, this function is  quite useful for sharing data on help lists.}

\value{ An object of the same type as \code{x}.
}

\author{Daniel Marcelino <dmarcelino@live.com>
}

\seealso{\code{\link{soundexBR}}.
}

\examples{

data(tobaccovote)

anonymize(tobaccovote)

anonymize(tobaccovote, keep.names=FALSE)
}

\keyword{ tables }

