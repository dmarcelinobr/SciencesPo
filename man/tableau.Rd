\encoding{UTF-8}
\name{tableau}
\alias{tableau}
\title{Proportion Table for Univariate Analysis
}
\description{Produces a latex-like table of the variable proportions.
}
\usage{
tableau(var, title=NULL)
}
\arguments{
  \item{var}{is the vector or the object to be computed proportions.}
  \item{title}{A character vector as title.}
}

\value{ A data frame object.
}


\author{Daniel Marcelino <dmarcelino@live.com>
}
\examples{

 x <- rep(1:5, 100)

cap <- "My Table With Proportions"

tableau(x, title = cap)
}

\keyword{ tables }
\keyword{ descriptive stats }
