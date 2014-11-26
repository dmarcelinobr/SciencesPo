\encoding{UTF-8}
\name{unload}
\alias{unload}
\title{Unload Package 
}
\description{Unload an uncessary package
}
\usage{
by.day(package, character.only = FALSE)
}
\arguments{
  \item{package}{A package names as string
}
  \item{character.only}{A logical string for character.only, default FALSE
}
}
\details{It works similarly as detach \dQuote{detach("package:name", unload=TRUE)}.
}
\author{
Daniel Marcelino
}

\seealso{\code{detach}
}
\examples{
require(foreign)
unload(foreign)
}
\keyword{ data manipulation }