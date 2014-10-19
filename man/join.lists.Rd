\encoding{UTF-8}
\name{join.lists}
\alias{join.lists}
\title{Join lists
}
\description{ Recursively join data frames 
}
\usage{
join.lists(dfs, ...)
}
\arguments{
  \item{dfs}{A list of data frames
}
  \item{\dots}{
Arguments passed onto merge.
}
}

\author{Daniel Marcelino
}
\examples{
mtcars$cars <- row.names(mtcars)
df1 <- mtcars[,c(1:2,12)]
df2 <- mtcars[,c(3:4,12)]
df3 <- mtcars[,c(5:6,12)]

join.lists(dfs=list(df1, df2, df3), by="cars")
}

\keyword{ data manipulation }
