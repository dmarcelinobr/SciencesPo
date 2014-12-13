\encoding{UTF-8}
\name{fade}
\alias{fade}
\title{Introduce transparency
}
\description{Alpha function to introduce transparency
}
\usage{
fade(color, alpha = .5)
}
\arguments{
  \item{color}{ Any color or vector of colors.
}
  \item{alpha}{Level for alpha.
}
}
\author{Daniel Marcelino <dmarcelino@live.com>
}

\examples{
# setup data
x <- seq(0, 50, 1)
supply <- x * -2 + 100
demand <- x * 2
# POINT SIZE AND TRANSPARENCY
plot(supply, demand, pch = 19, cex = 3, col = fade("red", 0.5))

}
\keyword{ graphs }