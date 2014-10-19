\encoding{UTF-8}
\name{toLatex}
\alias{toLatex}
\title{Generate Latex Code
}
\description{It links to the \sQuote{xtable} function for generating the latex code.
}
\usage{
toLatex(x, title = NULL)
}
\arguments{
  \item{x}{ is the object to be passed into latex code, usually a table object.
}
\item{title}{A vector with a caption to be pasted into the code.}
}

\value{ A latex code of the object \code{x}.

}
\references{David B. Dahl (2012) \emph{xtable: Export tables to LaTeX or HTML.}
}
\author{Daniel Marcelino <dmarcelino@live.com>
}
\note{It relies massively on the capabilities of \sQuote{xtable}.
}

\examples{

data(cgreene76)

tab <- detail(cgreene76, basic = TRUE)

toLatex(tab)

# Everything together 

cap <- "Presidential Election, Brazil (2006)"

toLatex(detail(cgreene76, basic = TRUE), cap)

}

\keyword{ tables }
