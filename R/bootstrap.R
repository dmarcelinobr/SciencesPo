#' @title Bootstrap
#' 
#' @description
#' This function is used for estimating standard errors when the distribution is not know.
#' 
#' @param x a vector.
#' @param boots The number of bootstraps.
#' @param fn the function you want to bootstrap, ie., mean, var, cov, etc.
#' 
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' 
#' @examples
#' x = runif(10, 0, 1)
#' bootstrap(x,fn=mean)
#' 
#' @export


bootstrap<-function(x, boots=100, fn){
	n=length(x)
	lings<-replicate(boots, fn(sample(x,n, replace=TRUE)))
	
	list(se=sd(lings), lings=lings)
}
