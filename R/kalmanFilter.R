#' @encoding UTF-8
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



#' @encoding UTF-8
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
#' # Different variances and means
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
