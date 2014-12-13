\encoding{UTF-8}
\name{by.year}
\alias{by.year}
\title{Yearly aggregation
}
\description{Yield for yearly aggregation. It does handle discontinuous time series by years, the user can also set its own method by passing an ad-hoc function for the aggregation method.
}
\usage{
by.year(data, FUN, years = NULL, na.rm = FALSE, civil = FALSE)
}
\arguments{
  \item{data}{A time-series object.
}
  \item{FUN}{A function for aggregating data.
}
  \item{years}{A numeric value for number of months to perform aggregation.
}
  \item{na.rm}{A logical value for na.rm, default is FALSE.
}
  \item{civil}{Wether the years start from the civil date or from the first observation in data. The default is civil = FALSE.}
}

\value{A data.frame of type long with dates formatted as as.Date and aggregated data.

}
\references{Ripley, B. D. and Hornik, K. (2001) Date-time classes. \emph{R News}, 1/2, 8–11 \url{http://www.r-project.org/doc/Rnews/Rnews_2001-2.pdf}.
}
\author{Daniel Marcelino
}

\seealso{\link{by.month}, \link{by.day}
}
\examples{
data(us2012)
\dontrun{Obama.ts <- get.ts(us2012[,4],  "%Y-%m-%d  %H:%M:%S", us2012[,8])
# yearly aggregated means for data:
yearly <-by.year(Obama.ts, mean)

# civil year arrangement: 
yearly <-by.year(Obama.ts, mean, civil=TRUE)

# Means aggregated for data:
yearly <-by.year(Obama.ts, mean, 2)
}
}

\keyword{ data manipulation }
