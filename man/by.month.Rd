\encoding{UTF-8}
\name{by.month}
\alias{by.month}
\title{ Monthly aggregation
}
\description{Yield for monthly aggregation. It does handle discontinuous time series by months, the user can also set its own method by passing an ad-hoc function for the aggregation method.
}
\usage{
by.month(data, FUN, months = NULL, na.rm = FALSE)
}
\arguments{
  \item{data}{A time-series object.
}
  \item{FUN}{A function to aggregate data.
}
  \item{months}{A numeric value for number of months to perform aggregation.
}
  \item{na.rm}{A logical value for na.rm, default is FALSE.
}
}
\value{A data.frame of type long with dates formatted as as.Date and aggregated data.

}
\references{Ripley, B. D. and Hornik, K. (2001) Date-time classes. \emph{R News}, 1/2, 8–11 \url{http://www.r-project.org/doc/Rnews/Rnews_2001-2.pdf}.
}
\author{
Daniel Marcelino
}

\seealso{\link{by.day}, \link{by.year}
}
\examples{
data(us2012)
Obama.ts <- get.ts(us2012[,4],  "%Y-%m-%d  %H:%M:%S", us2012[,8])
#monthly aggregated means for data:
monthly <-by.month(Obama.ts, mean)

#bimonthly or semimonthly aggregated means for data:
bimonthly <-by.month(Obama.ts, mean, 2)

}

\keyword{ data manipulation }
