\name{ggmodel}
\alias{ggmodel}
\title{ Diagnostic plots for models 
}
\description{Draw common plots for diagnostic of lm or glm models 
}
\usage{
ggmodel(model, toplot = "points", which = c(1:3, 5), mfrow = c(1, 1), what = "plots", ...)
}
\arguments{
  \item{model}{A model object
}
  \item{toplot}{What plot is in the top.
}
  \item{which}{ Which kind of plots will be shown.
}
  \item{mfrow}{A pattern to display the plots
}
  \item{what}{ Return what?
}
  \item{\dots}{graphical parameters to be passed to ggplot.
}
}
\references{
Modified from \url{http://librestats.com/2012/06/11/autoplot-graphical-methods-with-ggplot2/}
}
\author{
Daniel Marcelino
}
\seealso{ \code{\link{resplot}}
}
\examples{
y <- rnorm(100); x <- rnorm(100)
mod <- lm(y ~ x)
ggmodel(mod)
}

\keyword{ plots }
\keyword{ models }

