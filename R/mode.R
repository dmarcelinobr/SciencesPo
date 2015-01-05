#' @title  Calculate the Mode
#' 
#' @description Estimates the mode for a vector
#' 
#' @param x A data vector
#' @param na.rm A logical value, default is \code{FALSE}
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' 
#' @examples 
#' myvar <-c(1,1,2,2,3,3,4,4,5, NA)
#' mode(myvar)
#' 
#' mode(myvar, FALSE)
#' 
#' @export
#' 
mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = subset(x, !is.na(x))
  }
  y <- as.factor(x)
  freq <- summary(y)
  mode <- names(freq)[freq[names(freq)] == max(freq)]
  return(as.numeric(mode) )
}
