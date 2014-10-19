\encoding{UTF-8}
\name{add.footnote}
\alias{add.footnote}
\title{Footnote to ggplot object
}
\description{Adds a footnote to ggplot object
}
\usage{
add.footnote(text, size = 0.7, color = grey(0.75))
}
\arguments{
  \item{text}{ Any text or empty to use default.
}
  \item{size}{The size for text.
}
  \item{color}{Color for text.
}
}
\details{Only works with a ggplot object.
}
\author{Daniel Marcelino <dmarcelino@live.com>
}

\examples{
# setup data
x <- seq(0, 50, 1)
supply <- x * -2 + 100
demand <- x * 2
df <- data.frame( x = x, supply=supply, demand=demand)

library(ggplot2)
ggplot(df, aes(x)) + 
geom_line(aes(y = supply, colour = supply))+ 
geom_line(aes(y = demand, colour = demand))
add.footnote()
}
\keyword{ graphs }