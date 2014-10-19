\encoding{UTF-8}
\name{reclassify}
\alias{reclassify}
\title{Attribute column classes 
}
\description{
Coerces data.frame columns to the specified classes.}
\usage{
reclassify(data, classes)
}
\arguments{
  \item{data}{A data.frame object.
}
  \item{classes}{A vector of column attributes, one of: numeric, factor, character, etc.
}
}

\author{Daniel Marcelino}

\examples{ 
# make a data.frame
df <- data.frame(xvar = seq(1:10), yvar = rep(c("a","b"),5)) 
str(df)
reclassify(df, c("factor","factor"))

}

\keyword{ data manipulation }
