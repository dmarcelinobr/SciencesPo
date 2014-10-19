\encoding{UTF-8}
\name{sorter}
\alias{sorter}
\title{ Sort out a data frame
}
\description{
Sorts a data.frame by columns or rows or both.}

\usage{
sorter(data, var.order = names(data), col.sort = NULL)
}
\arguments{
  \item{data}{ A data object to sort.
}
  \item{var.order}{Variables to be ordered.
}
  \item{col.sort}{A column name or position to sort.
}
}
\details{
Can refer to variables either by names or number.
}
\author{
Daniel Marcelino
}

\examples{
data(montreal)
sorter(montreal, var.order = c("crimes07", "pop12to17y","quartier" ))

# sorter(montreal, var.order = c("crimes07", "pop12to17y"),
#      col.sort = c( "rate", "quartier"));
			
#sorter(montreal, var.order = c(5:7), # quartier crimes07 pop12to17y
# col.sort = 8) # SORT BY rate
}

\keyword{ data manipulation }
