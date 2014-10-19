\encoding{UTF-8}
\name{rename.files}
\alias{rename.files}
\title{Rename files recursively 
}
\description{Rename files in a directory with the same regex procedure.
}
\usage{
rename.files(dir, pattern, replacement)
}
\arguments{
  \item{dir}{
Path to directory for which you want to change the file names.
}
  \item{pattern}{
Pattern in file name to replace.
}
  \item{replacement}{
Text to replace pattern with.}
}

\value{
Prints "Operation complete" to console when done; look at files in directory 'dir' for changes.
}

\author{ Daniel Marcelino}

\examples{
# For example, a file name may have hyphen "-", and you can replace them with underline "_".
\dontrun{ rename.files("~/Desktop/mydata", "-", "_") }
}
\keyword{ data manipulation }
