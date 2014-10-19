\encoding{UTF-8}
\name{beta.parms}
\alias{beta.parms}
\title{Returns alpha and beta parameters
}

\description{Given mean and variance components, the function returns \eqn{\alpha} and \eqn{\beta} parameters for a Beta distribution.
}
\usage{
beta.parms(mu, var)
}
\arguments{
  \item{mu}{A value for mu.
}
  \item{var}{A value for var.
}
}
\details{It calculates \eqn{\alpha} and \eqn{\beta} parameters for the Beta distribution: 
\deqn{\alpha = \right(\frac{1-\mu}{\sigma^2}-\frac{1}{\mu}\right)\mu^2} and \deqn{\beta = \alpha\left(\frac{1}{\mu}-1\right)}
}
\value{A list object containing alpha and beta.
}
 \references{ N. Balakrishnan, V. B. Nevzorov (2003) \emph{A Primer on Statistical Distributions.} Wiley.
}
\author{Daniel Marcelino.
}
\note{ In R, the Beta distribution with parameters \eqn{shape1=\alpha} and \eqn{shape2=\beta} has density 
therfore, \code{dbeta(x, shape1=par[1], shape2=par[2]) }
In this parametrisation, the  mean is \deqn{E(X) = \frac{\alpha}{\alpha + \beta}}{E(X)=\alpha/\alpha+\beta} and the variance is \deqn{V(X) = \frac{\alpa \beta}{(\alpa + \beta)^2 (\alpa+\beta+1)} }.

}
\examples{

(par <- beta.parms(0.5, 0.1))

	}

\keyword{ ~models }
