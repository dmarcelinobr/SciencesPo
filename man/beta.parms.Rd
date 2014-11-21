\encoding{UTF-8}
\name{beta.parms}
\alias{beta.parms}
\title{Returns alpha and beta parameters
}

\description{Given mean and variance components, the function returns \eqn{\alpha} and \eqn{\beta} parameters to for a Beta distribution.
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
\details{It calculates \eqn{\alpha} and \eqn{\beta} parameters of the Beta distribution: 
\eqn{\alpha=\left(\frac{1-\mu}{\sigma^2}-\frac{1}{\mu}\right)\mu^2} and \eqn{\beta=\alpha\left(\frac{1}{\mu}-1\right)}.
}
\value{A list object containing alpha and beta.
}
 \references{ N. Balakrishnan, V. B. Nevzorov (2003) \emph{A Primer on Statistical Distributions.} Wiley.
}
\author{Daniel Marcelino.
}
\note{ In R, the Beta distribution with parameters \eqn{shape1=\alpha} and \eqn{shape2=\beta} has density 
therfore, \code{dbeta(x, shape1=par[1], shape2=par[2]) }
In this parametrisation, the  mean is  \eqn{E(X) = \frac{\alpha}{\alpha + \beta}} and the variance is \eqn{Var(X) = \frac{\alpha \beta}{(\alpha + \beta)^2 (\alpha+ \beta+1)}}.

}

\examples{

(par <- beta.parms(0.5, 0.1))

	}

\keyword{ models }
