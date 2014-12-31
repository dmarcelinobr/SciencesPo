#' @title Lag or Lead Observations
#' 
#' @description Shift function allows one to either lag or lead a column variables in a data frame
#' 
#' @param x The variable to be lagged or leaded
#' @param delta An integer for units to move backward or forward as negative (-#) or positive (#)
#' 
#' @return An object of the same type as \code{x}
#' 
#' @examples 
#' weather <- data.frame(
#' month = a <- c('J','F','M','A','M','J','J','A','S','O','N','D'),
#' precip = b <- c(78,62,74,78,76, 83, 91, 93, 93,78, 93, 81), 
#' max = c <- c(-6, -4, 2, 11, 19, 24,  26,  25, 20, 13, 5, -2),
#' min = d <- c(-15, -13, -7, 1, 8,  13, 16, 14, 9, 3, -2, -10) )
#' 
#' (weather$L.precip <- shift(weather$precip, -1) ) # lag
#' 
#' (weather$precip.L <- shift(weather$precip, 1) ) # lead
#' 
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' 
#' @export
shift <-
function(x,delta=NA){
  stopifnot(is.numeric(delta))
  stopifnot(is.numeric(x))
  
  if (length(delta)>1)
    return(sapply(delta,shift, x=x))
  
  output<-NULL
  abs.delta=abs(delta)
  if (delta > 0 )
    output<-c(tail(x,-abs.delta),rep(NA,abs.delta))
  else if (delta < 0 )
    output<-c(rep(NA,abs.delta), head(x,-abs.delta))
  else 
    output <- x
  return(output)
}
