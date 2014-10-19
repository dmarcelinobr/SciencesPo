\encoding{UTF-8}
\name{dummy}
\alias{dummy}
\title{Generate Dummy Variables

}
\description{Provides an alternative to generate dummy variables.

}
\usage{
dummy(x, data = NULL, drop = TRUE)
}
\arguments{
  \item{x}{A vector or variable to produce dummies.
}
  \item{data}{A data object as a data frame.
}
  \item{drop}{A logical value. If TRUE, unused levels will be omitted
}
}

\value{A matrix object

}
\author{
Daniel Marcelino <dmarcelino@live.com>
}

\examples{
data(ltaylor96)
dummy(ltaylor96$year)
}
\keyword{ attributes }
\keyword{ models }
