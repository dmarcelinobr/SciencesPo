\encoding{UTF-8}
\name{montreal}
\alias{montreal}
\docType{data}
\title{Juvenile Crimes in Montreal
}
\description{Data set containing number of crimes committed by teenagers in Montreal in 2007. I have added spatial coordinates to allow for spatial analysis.
}
\usage{data(montreal)}
\format{
  A data frame with 34 observations on the following 8 variables.
  \describe{
    \item{\code{postal}}{a character vector for the postal code}
    \item{\code{city}}{Montreal}
    \item{\code{lon}}{Longitude coordinates}
    \item{\code{lat}}{Latitude coordinates}
    \item{\code{quartier}}{Police station neighbourhood.}
    \item{\code{crimes07}}{Crimes in 2007 involving adolescents}
    \item{\code{pop12to17y}}{Proportion of juveniles (12 to 17 years old) in the neighbourhood.}
    \item{\code{rate}}{Criminality rate = crimes07/pop12to17y}
  }
}
\source{Report of the PVM--Montreal Police Service (2007). 
}

\examples{
data(montreal)

}
\keyword{datasets}
