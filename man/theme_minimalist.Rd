\encoding{UTF-8}
\name{theme_minimalist}
\alias{theme_minimalist}
\title{ggplot2 minimalist 
}
\description{
Applies a minimalist to a ggplot2 object}
\usage{
theme_minimalist(base_size = 12, base_family = "sans", title_family = "mono")
}
\arguments{
  \item{base_size}{
Font size.
}
  \item{base_family}{Font type.
}
  \item{title_family}{Font type for title.
}
}
\value{ggplot2 graph style. 
}
\references{
	Wickham, Hadley ()2009) \emph{ggplot2: elegant graphics for data analysis,} Springer, New York.
  }
\author{
Daniel Marcelino <dmarcelino@live.com>
}
\note{To be used with ggplot2.

}
\examples{
# setup data
x <- seq(0, 50, 1)
supply <- x * -2 + 100
demand <- x * 2
df <- data.frame( x = x, supply=supply, demand=demand)

library(ggplot2)
ggplot(df, aes(x)) + 
  geom_line(aes(y=supply, colour="supply")) + 
  geom_line(aes(y=demand, colour="demand")) +
theme_minimalist()
	
	}
\keyword{ ~graphs }