#' @title Rescaling as Z-scores and Centering 
#' 
#' @description  Provides a rapidly way to generate standard variables.
#' 
#' @param x A vector to be computed
#' @param range A valid range
#' 
#' @return Scaled values in an object the same class as \code{x}.
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#'
#'
#' @examples
#' x <- sample(10)
#' rescale(x, range=0:1)
#' rescale(x, 0:1)
#' 
#' @keywords Standardization
#'
#' @seealso  \code{\link{center}}, \code{\link{standardize}}
#' @export
#'
rescale <-function(x, range) {
    if(nargs() > 1 && is.numeric(x) && is.numeric(range)) {
        # if newrange has max first, reverse it
        if(range[1] > range[2]) {
            newmin<-range[2]
            range[2]<-range[1]
            range[1]<-newmin
        }
        xrange<-range(x)
        if(xrange[1] == xrange[2]) stop("cannot rescale a constant vector!")
        mfac<-(range[2]-range[1])/(xrange[2]-xrange[1])
        return(range[1]+(x-xrange[1])*mfac)
    }
    else {
        cat("Usage: rescale(x,0:1)\n")
        cat("\twhere `x` is a numeric object and range is the `min` and `max` of the new range\n")  }
}