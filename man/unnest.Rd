\encoding{UTF-8}
\name{unnest}
\alias{unnest}

\title{
Unnest lists.}
\description{Unnest a nested list.

}
\usage{
unnest(x)
}
\arguments{
  \item{x}{A nested list.
}
}
\value{A list, with no nesting, hopefully. 
}

\author{Daniel Marcelino	
}
\examples{
# a nested list
mylist <- list(); inerlist <- list()
for(i in 1:5) {
for(j in 1:5) {
mylist[[j]] <- i*j
} 
 inerlist[[i]] <- mylist
 }

# Unnest the list
unnest(inerlist)[[1]]
}
\keyword{ data manipulation }
