\encoding{UTF-8}
\name{bysort}
\alias{bysort}
\title{ Sort out a data frame
}
\description{
Sorts a data.frame by columns or rows or both.}

\usage{
bysort(data, vars = NULL, var.order = names(data))
}
\arguments{
  \item{data}{ A data object to sort.
  }
    \item{vars}{A column name or position to sort.
  }
  \item{var.order}{Variables to be ordered.
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
# bysort(montreal, var.order = c("crimes07", "pop12to17y","quartier" ))

#bysort(montreal, vars = c( "rate", "quartier"), var.order = c("crimes07", "pop12to17y"));
			
bysort(montreal, vars = 8) # SORT BY rate
}

\keyword{ data manipulation }
