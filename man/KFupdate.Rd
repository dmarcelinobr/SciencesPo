\encoding{UTF-8}
\name{KFupdate}
\alias{KFupdate}
\title{Kalman Filter Update
}
\description{Estimates the Kalman Filtering 

}
\usage{
KFupdate(mean1, var1, mean2, var2)
}
\arguments{
  \item{mean1}{Prior mean. }
  \item{var1}{ Prior variance. }
  \item{mean2}{ New mean. }
\item{var2}{New variance. }
}
\references{
Kalman, R.E. (1960) A new approach to linear filtering and prediction problems. \emph{Journal of Basic Engineering} \bold{82 (1):} 35–45.
}
\author{Daniel Marcelino
}

\examples{
# Equal variances but different means
KFupdate(10, 4, 12, 4) 

# Different variances and means
KFupdate(10, 8, 13, 2) 
#measurements = c(5, 6, 7, 9, 10)
#motion = c(1, 1, 2, 1, 1)
#measurement_sig = 4
#motion_sig = 2
#mu = 0
#sig = 10000


#for n in range(length(measurements)){
# [mu, sig] <- update(mu, sig, measurements[n], measurement_sig)  
# return( [mu, sig])
# [mu, sig] <- predict(mu, sig, motion[n], motion_sig)
# return( [mu, sig]) }
}

\keyword{ models }
