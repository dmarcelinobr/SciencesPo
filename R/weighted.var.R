#' @title Weighted Variance
#'
#'@description Weighted Variance Formula
#'
#'@param x the varaible 
#'@param w the variance
#'
weighted.var <- function(x, w) 
  return(sum(w * (x - weighted.mean(x,w))^2)/((length(x)-1)*mean(w)))
  
