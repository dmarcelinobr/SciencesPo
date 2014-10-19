\name{skewness}
\alias{skewness}
\title{Test for Skewness
}
\description{The function provides three features to perform a skewness test, see details below.
}
\usage{
skewness(x, na.rm = TRUE, type = 2)
}
\arguments{
  \item{x}{A numeric vector containing the values whose skewness is to be computed.
}
  \item{na.rm}{A logical value indicating whether NA values should be stripped before the computation proceeds.
}
  \item{type}{An integer between 1 and 3 selecting one of the algorithms for computing skewness detailed below.
}
}
\details{The skewness is a measure of symmetry distribution. Intuitively, negative skewness (g_1 < 0) indicates that the mean of the data distribution is less than the median, and the data distribution is left-skewed. Positive skewness (g_1 > 0) indicates that the mean of the data values is larger than the median, and the data distribution is right-skewed. Values of g_1 near zero indicate a symmetric distribution. The skewness function will ignore missing values in \sQuote{x} for its computation purpose. There are several methods to compute skewness, Joanes and Gill (1998) discuss three of the most traditional methods. According to them, \bold{type 3} performs better in non-normal population distribution, whereas in normal-like population distribution type 2 fits better the data. Such difference between the two formulae tend to disappear in large samples.

\bold{Type 1:} g_1 = m_3/m_2^(3/2). 

\bold{Type 2:} G_1 = g_1*sqrt(n(n-1))/(n-2). 

\bold{Type 3:} b_1 = m_3/s^3 = g_1 ((n-1)/n)^(3/2).

}
\value{An object of the same type as \code{x}.

}
\references{
Joanes, D. N. and C. A. Gill. (1998) Comparing measures of sample skewness and kurtosis. \emph{The Statistician,} \bold{47,} 183--189.
}
\author{Daniel Marcelino <dmarcelino@live.com>
}

\seealso{\code{\link{detail}}.
}
\examples{

skewness(c(100,200,300), type = 2)

skewness(c(100,200,300), type = 1)

skewness(c(100,200,300), type = 3)

w<-sample(4,10, TRUE)

x <- sample(10, 1000, replace=TRUE, prob=w)

skewness(x, type=2)

skewness(x, type=3)

detail(x)
}

\keyword{ descriptive stats }
