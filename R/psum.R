#' @title Parallel sum
#' 
#' @description Provides parallel sum like \code{pmin} and \code{pmax} from the base package. The function \code{sum} simply does not help when the objective is to obtain a vector with parallel sum rather than a scalar value.
#' 
#' @param \dots One or more unit objects
#' @param na.rm A logical value \code{TRUE} or \code{FALSE}, the default
#' 
#' @return A vector containing the parallel sum.
#' 
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' 
#' @keywords Misc
#' 
#' @examples 
#' data(us2012)
#' psum(us2012$Obama, us2012$Romney)
#' Swingy <-psum(us2012$Obama, us2012$Romney-100)
#' 
#' @export
psum <-
function(..., na.rm=FALSE) { 
x <- list(...)
 rowSums(matrix(unlist(x), ncol=length(x)), na.rm=na.rm)
 }
