\encoding{UTF-8}
\name{outtags}
\alias{outtags}
\title{Remove String Tags
}
\description{Clean up string tags
}
\usage{
outtags(x, tab = FALSE)
}
\arguments{
  \item{x}{is a vector or a data frame containing string tags.}
  \item{tab}{a logical value indicating whether tab spaces should be treated as extraneous characters.}
}
\value{An object of the same type as \code{x}.
}

\author{Daniel Marcelino <dmarcelino@live.com>
	}

\seealso{ \code{\link{accent}}.
}
\examples{

tags <- ('These tags should not be here & # a  .')

outtags(tags)

}

