\encoding{UTF-8}
\name{detail}
\alias{detail}
\title{Descriptive Statistics Table
}
\description{ This function provides up to 14 statistics for an entire data object: number of cases, mean, standard deviation, variance, standard error, median, mad (median absolute deviation), trimmed and winsorized means, range, minimum, maximum, skewness, and kurtosis.
}
\usage{
detail(x, basic=FALSE,na.rm=TRUE,trim=.2,type=2,k=1)
}
\arguments{
  \item{x}{ A vector or a data frame.
}
  \item{basic}{A logical value indicating whether only a short version of the descriptive table might be returned. }
  \item{na.rm}{A logical value indicating whether NA values should be stripped before the computations.}
  \item{trim}{Is the proportion of the data to be replaced for estimating the average.}
  \item{type}{A numeric value (fraction) to be trimmed. The value in trim will be discarded from the top and bottom of data. See in details below.}
  \item{k}{A numeric value for observations in the data set to be discarded while computing the winsorized mean. See details below.}
}
\details{Trimming is not  Winsorizing, which is more complex than simply excluding data. In a trimmed estimator, the extreme values are discarded; in a Winsorized estimator, the extreme values are instead replaced by certain percentiles (the trimmed minimum and maximum)
}
\value{ A data frame containing the require computations. 
}
\references{
Agresti, A., and Finlay, B. (1997) \emph{Statistical methods for the social sciences.}

Peter J. Huber (2002)  John W. Tukey's Contributions to Robust Statistics.  \emph{The Annals of Statistics,} \bold{30(6),} 1640--1648.

Tukey, J. W. (1962) The Future of Data Analysis. \emph{The Annals of Mathematical Statistics}, \bold{33,} p. 18.

Venables, W. N. and Ripley, B. D. (2002) \emph{Modern Applied Statistics with S.}. Springer.
}


\author{
	Daniel Marcelino <dmarcelino@live.com>
}


\seealso{\code{\link{cgreene76}}.  
}
\examples{
data(cgreene76) #load some data

 # To apply the function
 
detail(cgreene76, trim = 0.5, k = 3)
}

\keyword{ descriptive stats }
\keyword{ tables }
