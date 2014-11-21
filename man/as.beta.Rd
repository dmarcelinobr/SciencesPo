\encoding{UTF-8}
\name{as.beta}
\alias{as.beta}
\title{Standardized Beta Coefficients
}
\description{Generate standardized beta coefficients from an adjusted model. Standardized coefficients refer to how many standard deviations a dependent variable will change per standard deviation increase in the predictor variable. See details.
}
\usage{
as.beta(mod, stdev)
}
\arguments{
  \item{mod}{a linear object model.}
  	 \item{stdev}{a numeric value for standard deviation, default is 1.}
}

\details{Standardized beta coefficients are values estimated from unstandardized coefficients, which are only partially adjusted by the quotient of the standard deviation of an independent variable, and the standard deviation of a dependent variable. Essentially, beta coefficients report the relative importance of each independent variables. Then, this standardization is rather useful for multiple regression models, once it may aid on identifying critical IV.
}

\value{A standardized coefficient.}

\references{
Baguley, T. (2009) Standardized or simple effect size: What should be reported?. \emph{British Journal of Psychology,} \bold{100(3),} 603-617.

Gelman, A. (2008) Scaling regression inputs by dividing by two standard deviations. \emph{Statistics in Medicine}, \bold{27:} 2865–2873.

Gelman, A., and Hill, J. (2006) \emph{Data analysis using regression and multilevel/hierarchical models.} Cambridge University Press.
}

\author{Daniel Marcelino <dmarcelino@live.com>
}

\examples{
df <- read.csv("http://www.math.smith.edu/r/data/help.csv") #get some data

female <- subset(df, female==1)

#fit a linear regression (OLS) model.

(model <- lm(pcs ~ mcs + homeless, data=female) )

#finally, apply standardization to the coefficients.

as.beta(model)

# Possible interpretation: a change in 1 standard deviation of mcs has more than 
# twice the impact on pcs than a 1 standard deviation change in the homeless variable.
# This example points up another potential weakness of standardized regression 
# coefficients, however, in that the homeless variable can take on values of 
# 0 or 1, and a 1 standard deviation change is hard to interpret.

}

\keyword{ standardization }
\keyword{ models }
