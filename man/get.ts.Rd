\name{get.ts}
\alias{get.ts}
\title{Time series parameters made simple 
}
\description{Returns a data frame with time parameters and time variable.
}
\usage{
get.ts(datevar, format, datavar = NULL, tz = "GMT")
}
\arguments{
  \item{datevar}{A time variable}
  \item{format}{A time format 
}
  \item{datavar}{data variable to convert to a time series
}
  \item{tz}{Time zone code	
}
}
\value{A data frame containing time parameters and measuring variable.
}

\author{
Daniel Marcelino
}
\examples{
data(us2012)
\dontrun{get.ts(datevar = us2012[,3], format = "%Y-%m-%d", datavar = us2012[,9])
}
}

\keyword{ data manipulation }
