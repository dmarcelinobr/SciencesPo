#' @title Kalman Filter Predict
#' 
#' @description Estimates the Kalman Filtering 
#' 
#' @param mean1 Prior mean
#' @param var1 Prior variance
#' @param mean2 New mean
#' @param var2 New variance
#'   
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' 
#' @examples
#' # Equal variances but different means
#' kalmanPredict(10, 4, 12, 4) 
#' 
#' @keywords Models
#' @export
kalmanPredict <- function(mean1, var1, mean2, var2){
  newmean = mean1 + mean2
  newvar = var1 + var2
  return(c(newmean,newvar))
}
NULL




#' @title Kalman Filter Update
#' 
#' @description Replicate the Kalman Filtering Process
#'
#' @param mean1 Prior mean
#' @param var1 Prior variance
#' @param mean2 New mean
#' @param var2 New variance
#'
#' @references Kalman, R.E. (1960) A new approach to linear filtering and prediction problems. \emph{Journal of Basic Engineering} \bold{82 (1):} 35–45.
#' 
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @examples
#' # Equal variances but different means
#' kalmanUpdate(10, 4, 12, 4) 
#'
# Different variances and means
#' kalmanUpdate(10, 8, 13, 2)
#' 
#' @keywords Models
#' @export
kalmanUpdate <- function(mean1, var1, mean2, var2){
  newmean = (var2 * mean1 + var1 * mean2) / (var1 + var2)
  newvar = 1 / (1/var1 + 1/var2)
  return(c(newmean,newvar))
}
NULL





# you can fix your code with 'assign':
#  
#  sumfill = function(I,J) { for (i in 1:I) for (j in 1:J) 
#    matTest[i,j]=i+j; assign("matTest", matTest, parent.frame()) } 
#sumfill(3,3) 
# matTest is filled 
#
# a generic version of sumfill, if that's what you need, could be implemented as 
#
# sumfill = function(m) { n=deparse(substitute(m)); d=dim(m); for (i in 1:d[1]) for (j in 1:d[2]) m[i,j]=i+j; assign(n,m,parent.frame()) } 
#
#
#... but the preferred style, i think, would be sth like: 
#sumfill = function(I,J) { m=matrix(nrow=I,ncol=J); for (i in 1:I) for 
#(j, 1:J) m[i,j]=i+j; m } 
#testMat = sumfill(3,3) 

