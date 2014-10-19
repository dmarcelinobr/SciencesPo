\encoding{UTF-8}
\name{get.vif}
\alias{get.vif}
\title{
Variance Inflation Factor
}
\description{Extracts Variance Inflation Factor from a model.
}
\usage{
get.vif(model, ...)
}
\arguments{
  \item{model}{A model object.
}
\item{\dots}{ Options to pass to
}
}
\value{A numeric value.
}
\author{
Daniel Marcelino
}
\examples{
data()
m1 <- lm(mpg ~ qsec + hp, data=mtcars)
get.vif(m1)
}

\keyword{ models }