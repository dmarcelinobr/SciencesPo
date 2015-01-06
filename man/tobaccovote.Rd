\encoding{UTF-8}
\name{tobaccovote}
\alias{tobaccovote}
\docType{data}
\title{Tobacco Lobbying Data
}
\description{Voting on tobacco-related legislation by members of the US Congress from 1997 to 2000.
}
\usage{data("tobaccovote")}
\format{
  A data frame with 7533 observations on the following 10 variables.
  \describe{
    \item{\code{id}}{  ID.}
    \item{\code{state}}{States.} 
    \item{\code{lastname}}{Congress member's name.} 
    \item{\code{bilindx}}{a numeric vector} 
    \item{\code{votedpro}}{Dummy if voted pro or anti tobacco.}
    \item{\code{party}}{Party name.}
    \item{\code{money}}{PAC contributions (\$K).}
    \item{\code{acres}}{The number of harvested acres of tobacco in 1999, in thousands of acres.}
    \item{\code{gsp}}{a numeric vector}
    \item{\code{lobby}}{a numeric vector}
  }
}
\details{Data set used in the book: \sQuote{Multilevel Modeling} by Douglas Luke. 
}
\source{ Received from the author eletronically in 2009.
}
\references{
	Luke, Douglas (2004) \emph{Multilevel Modeling,} Sage, Thousdand Oaks.
}

\examples{
data(tobaccovote)
}
\keyword{datasets}
