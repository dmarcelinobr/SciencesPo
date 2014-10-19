\encoding{UTF-8}
\name{soundexBR}
\alias{soundexBR}
\title{Soundex Encoding For Portuguese BR
}
\description{SoundexBR returns a census-like soundex code a string name, given the Portuguese (Brazilian) sound system. This function was firstly outlined to work with \code{RecordLinkage} package, however, it also is helpful as a standalone function. See details bellow.
}
\usage{
soundexBR(term)
}
\arguments{
  \item{term}{a list, a vector or a data frame with strings.
}
}
\details{This function assign a `soundex' code for strings based on their sounds, instead of their spelling. For instance, names that sound alike, but spelled differently, like \bold{SOUZA} and \bold{SOUSA}, will be assigned to an identically code. Therefore, this function may help on finding a surnanames even if they were registered with minor misspellings. The code consists of 4 digits long: a letter and three numbers as \dQuote{0-000} <capital letter><digit><digit><digit>. The integers refers to the remaining letters.

}
\value{A character vector or matrix with the same dimensions as \code{term}.
}
\references{
Borg, Andreas and Murat Sariyar. (2012) \emph{RecordLinkage: Record Linkage in R,} R package version 0.4-1,  \url{http://CRAN.R-project.org/package=RecordLinkage}.

Camargo Jr. and Coeli CM. (2000) Reclink: aplicativo para o relacionamento de bases de dados, implementando o método probabilistic record linkage. \emph{Cad. Saúde Pública}, \bold{16(2)}, Rio de Janeiro.

Marcelino, Daniel (2013) \emph{SciencesPo: A Tool Set for Analyzing Political Behaviour Data, \url{(http://dx.doi.org/10.2139/ssrn.2320547}.}

}
\author{Daniel Marcelino <dmarcelino@live.com>
}
\note{This function was adapted from the US census soundex version. 
See in \url{http://archives.gov/research/census/soundex.html}

The genealogist Dick Eastman maintains a soundex calculator in his website at \url{http://eogn.com/soundex/}.

}

\seealso{\code{\link{soundexES}}, \code{\link{soundexFR}}.
}
\examples{

# A silly example:
names <- c('Ana Karolina Kuhnen',
'Ana Carolina Kuhnen', 'Ana Karolina',
'Dilma Vana Rousseff', 'Dilma Rousef')
  
soundexBR(names)

# Example with RecordLinkage:
#Some data:
mydata1 <- data.frame(
fname=c('Ricardo','Maria','Tereza','Pedro','José', 'Germano'),
lname=c('Cunha','Andrade','Silva','Soares','Silva','Lima'),
age=c(67,89,78,65,68,67),
birth=c(1945,1923,1934,1947,1944,1945),
date=c(20120907,20120703,20120301,20120805,20121004,20121209))


mydata2<-data.frame(
fname=c('Maria','Lúcia','Paulo','Marcos', 'Ricardo', 'Germânio'),
lname=c('Andrada','Silva','Soares','Pereira','Cunha','Lima'),
age=c(67,88,78,60,68,80),
birth=c(1945,1924,1934,1952,1944,1932),
date=c(20121208,20121103,20120302,20120105,20121004,20121209))

# Must call RecordLinkage package

\dontrun{pairs<-compare.linkage(mydata1, mydata2,
blockfld=list(c(1,2,4),c(1,2)),
phonetic<-c(1,2), phonfun = soundexBR, strcmp = FALSE,
strcmpfun<-jarowinkler, exclude=FALSE,identity1 = NA,
identity2=NA, n_match <- NA, n_non_match = NA)
      
print(pairs)

editMatch(pairs)

# To access information in the object:  
weights <- epiWeights(pairs, e = 0.01, f = pairs$frequencies)
hist(weights$Wdata, plot = FALSE) # Plot TRUE
getPairs(pairs, max.weight = Inf, min.weight = -Inf)
	}
}

\keyword{ soundex  }
