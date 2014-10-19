\encoding{UTF-8}
\name{winsor.mean}
\alias{winsor.mean}
\title{ Winsorized Mean 
}
\description{This function computes the winsorized mean. The winsorization approach consists of recoding the top k values.
}
\usage{
winsor.mean(x, k = 1, na.rm = TRUE)
}
\arguments{
  \item{x}{ is the vector to be winsorized.
}
  \item{k}{is an integer for the quantity of outlier elements that should be replaced to the computation purpose.
}
  \item{na.rm}{A logical value indicating whether NA values should be stripped before the computations.
}
}
\details{Winsorizing a vector will produce different results than trimming it. By trimming a vector, the extreme values are discarded, while by winsorizing it the extreme values are replaced by certain percentiles.

}
\value{ An object of the same type as \code{x}.

}
\references{

Dixon, W. J., and Yuen, K. K. (1999) Trimming and winsorization: A review. \emph{The American Statistician,} \bold{53(3),} 267--269.

Dixon, W. J., and Yuen, K. K. (1960) Simplified Estimation from Censored Normal Samples, \emph{The Annals of Mathematical Statistics,} \bold{31,} 385--391.

Wilcox, R. R. (2012) \emph{Introduction to robust estimation and hypothesis testing.} Academic Press, 30-32.

Statistics Canada (2010) \emph{Survey Methods and Practices.}
}

\author{Daniel Marcelino <dmarcelino@live.com>
}
\note{One may want to winsorize estimators, however, winsorization tends to be used for one-variable situations.
}


\seealso{\code{\link{detail}}.
}
\examples{
x <- rnorm(100)

winsor.mean(x)

# see this function in context. 
detail(x) 

}

\keyword{ descriptive stats }

