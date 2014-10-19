\encoding{UTF-8}
\name{rstack}
\alias{rstack}
\title{ Produces a Stacked Table With Correlations}
\description{Pearson's product-moment coefficient is the well known degree to which two or more variables are linearly associated. In a two-dimensional table, the degree of correlation among the variables is quantified by the correlation coefficient.

}
\usage{
	rstack(m)
}
\arguments{
  \item{m}{is a square matrix.}
}
\details{The function generates a flatten table of correlations. Using it beside \code{\link{toLatex}} will produce a latex-like correlation table. If necessary, typing \sQuote{cor(t(x))} will coerce \sQuote{x} to a properly square matrix format. 
}
\value{An table or a data.frame containing the correlations for \code{m}.

}
\references{

Agresti, A., and Finlay, B. (1997) \emph{Statistical methods for the social sciences.}

Aldrich, John (1995) Correlations Genuine and Spurious in Pearson and Yule. \emph{Statistical Science,} \bold{10(4),} 364--376.

Spiegel, M. R. (1992) Correlation Theory. in: \emph{Theory and Problems of Probability and Statistics,} 2nd ed. New York: McGraw-Hill, pp. 294--323.

}
\author{Daniel Marcelino <dmarcelino@live.com>
}
\note{When using the function to correlate values of a variable with corresponding values at a different time will lead to an autocorrelation table.
}

\seealso{\code{\link{toLatex}},\code{\link{rprob}}. 
}
\examples{

data(nerlove63)

(table <- rprob(nerlove63) )

rstack(table)

toLatex(rstack(table))

}


\keyword{ tables }
\keyword{ descriptive stats }

