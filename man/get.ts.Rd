\name{get.ts}
\alias{get.ts}
\title{Time series parameters made simple 
}
\description{Returns a data frame with time parameters and time variable.
}
\usage{
get.ts(timevar, format, data = NULL, tz = "GMT")
}
\arguments{
  \item{timevar}{A time variable}
  \item{format}{A time format 
}
  \item{data}{data variable to convert to a time series
}
  \item{tz}{Time zone code, default is GMT.
}
}
\value{A data frame containing time variable parameters and measuring variable.
}
\references{Ripley, B. D. and Hornik, K. (2001) Date-time classes. \emph{R News}, 1/2, 8–11 \url{http://www.r-project.org/doc/Rnews/Rnews_2001-2.pdf}.
}
\author{
Daniel Marcelino
}
\examples{
data(us2012)

myts <- get.ts(timevar = us2012[,4], format = "%Y-%m-%d  %H:%M:%S", data = us2012[,8])
h2f(myts)

}

\keyword{ data manipulation }
