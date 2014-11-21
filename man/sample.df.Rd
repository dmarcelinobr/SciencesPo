\name{sample.df}
\alias{sample.df}
\title{Extract a simple random sample from a data frame}
\usage{
sample.df(df, n, replace = FALSE, vars = NULL)
}
\arguments{
\item{df}{The data frame to be sampled from.}

\item{n}{Number of observations (rows) to sample.}

\item{replace}{A logical indicating whether sampling with replacement or not (default).}

\item{vars}{A list of variables to include in the
sampled data, the default is to include all}
}
\value{
Returns the same object as \code{df}.  Optionally,
only includes the columns listed in \code{vars}.
}
\description{
Extract a simple random sample from a data.frame.
}
\examples{
data(tobaccovote)

# sample all variables without replacement
( ans1 <- sample.df(tobaccovote,10) )

# sample two variables without replacement
( ans2 <- sample.df(tobaccovote,25,vars=c("votedpro","party")) )

# sample two variables with replacement
( ans3 <- sample.df(tobaccovote,25,replace=TRUE,vars=c("votedpro","party")) )
}

\keyword{descriptive stats}

