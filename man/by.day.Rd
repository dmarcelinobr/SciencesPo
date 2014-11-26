\encoding{UTF-8}
\name{by.day}
\alias{by.day}
\title{Daily aggregation
}
\description{Yield for daily aggregation. It does handle discontinuous time series by days and weeks, the user can also set its own method by passing an ad-hoc function for the aggregation method. 
}
\usage{
by.day(data, FUN, days = NULL, na.rm = FALSE)
}
\arguments{
  \item{data}{A time-series object.
}
  \item{FUN}{A function to aggregate data. 
}
  \item{days}{A numeric value for number of days to perform aggregation
}
  \item{na.rm}{A logical value for na.rm, default is FALSE.
}
}
\details{To weekly aggregation, set \code{days = 7}.
}
\value{ A data.frame of type long with dates formatted as as.Date and aggregated data.
}
\references{ Ripley, B. D. and Hornik, K. (2001) Date-time classes. \emph{R News}, 1/2, 8–11 \url{http://www.r-project.org/doc/Rnews/Rnews_2001-2.pdf}.
}
\author{
Daniel Marcelino
}

\seealso{\link{by.month}, \link{by.year}
}
\examples{
data(us2012)
Obama.ts <- get.ts(us2012[,4],  "%Y-%m-%d  %H:%M:%S", us2012[,8])
#Daily aggregate means for data:
daily <-by.day(Obama.ts, mean)

#Weekly aggregated means for data:
weekly <-by.day(Obama.ts, mean, 7)

}
\keyword{ data manipulation }