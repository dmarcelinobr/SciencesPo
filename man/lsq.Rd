\encoding{UTF-8}
\name{lsq}
\alias{lsq}
\title{Latin Square
}
\description{A Latin square is a matrix of n*n elements. That is, it consists in a matrix of n columns and n rows arranged in a way to avoid orthogonality (a row or a column will not contain the same number twice).
}
\usage{
lsq(size, rep = 1, seed = NA, id = FALSE)
}
\arguments{
  \item{size}{An integer value for the size of the design.}
  \item{rep}{An integer value for repetition design.}
  \item{seed}{A value to generate a random distribution. The argument seed is useful for it assures the same distribution in case replication is needed.}
  \item{id}{A logical value indicating whether an ID should be attributed for each element of the matrix.}
}

\value{ A Latin square object with class \code{matrix}.
}

\references{

Tarry, G. (1900) Le probleme de 36 officiers. Compte Rendu de l'Assoc. \emph{Francais Avanc. Sci. Naturel,} \bold{1,} 122--123.

Tarry, G. (1901) Le probleme de 36 officiers. Compte Rendu de l'Assoc. \emph{Francais Avanc. Sci. Naturel,} \bold{2,} 170--203.
}
\author{Daniel Marcelino <dmarcelino@live.com>
}
\examples{

lsq(4, 2, seed = 51)

}


\keyword{ simulations }
