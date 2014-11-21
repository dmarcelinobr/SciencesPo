\name{toTitle}
\alias{toTitle}
\title{Convert to Title Case
}
\description{Capitalize the first letter of each name.
}
\usage{
toTitle(x)
}
\arguments{
  \item{x}{ A name or a vector containing names.
}
}
\value{
The same as input.
}

\author{
Daniel Marcelino
}
\examples{
mytitle <- c("SciencesPo", "a tool set for political behaviour data")

sapply(mytitle, toTitle)

toTitle("a tool set for political behaviour data")

}

\keyword{ strings }
