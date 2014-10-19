\encoding{UTF-8}
\name{resplot}
\alias{resplot}
\title{ Plot of residues 
}
\description{Produces a nice residues plot using ggplot
}
\usage{
resplot(model)
}
\arguments{
  \item{model}{A linear model object
}
}

\references{
Hadley Wickham (2009) \emph{ggplot2: elegant graphics for data analysis.} Springer, New York.

}
\author{
Daniel Marcelino
}

\examples{
y <- rnorm(100); x <- rnorm(100)
mod <- lm(y ~ x)
resplot(mod)

}

\keyword{ descriptive stats }
