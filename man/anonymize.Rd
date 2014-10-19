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

weather <- data.frame(
month = a <- c('J','F','M','A','M','J','J','A','S','O','N','D'),
precip = b <- c(78,62,74,78,76, 83, 91, 93, 93,78, 93, 81), 
max = c <- c(-6, -4, 2, 11, 19, 24,  26,  25, 20, 13, 5, -2),
min = d <- c(-15, -13, -7, 1, 8,  13, 16, 14, 9, 3, -2, -10) 
)

anonymize(weather)

anonymize(weather, keep.names=FALSE)
}

\keyword{ tables }

