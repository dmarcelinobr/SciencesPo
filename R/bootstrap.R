#' @encoding UTF-8
#' @title Method for Bootstrap
#'
#' This method is intended to be provides statistical models that support  bootstrapping.
#' @param x a a vector or a fitted model object that will be used to produce bootstrapped parameters. Model objects are from the class \dQuote{glm} or \dQuote{lm}.
#' @param  \dots unspecified parameters
#' @return a list with the \dQuote{alpha} and \dQuote{beta} slots set. Note that \dQuote{alpha} corresponds to ancillary parameters and \dQuote{beta} corresponds to systematic components of the model.
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @export
bootstrap <- function (x, ...)
  UseMethod("bootstrap")

#' @title Bootstrap
#' 
#' @description
#' This function is used for estimating standard errors when the distribution is not know.
#' 
#' @param boots The number of bootstraps.
#' @param FUN the statistic to bootstrap, ie., mean, var, cov, etc.
#' 
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' 
#' @examples
#' x = runif(10, 0, 1)
#' bootstrap(x,FUN=mean)
#' 
#' @rdname bootstrap
#' @export
bootstrap.default<-function(x, boots=100, FUN,  ...){
	n=length(x)
	lings <-replicate(boots, FUN(sample(x, n, replace=TRUE)))
	list(se = sd(lings), 
       lings = lings)
}
NULL


#' @title Bootstrap Parameters of a Statistical Model
#'
#' This method is used for bootstrapping statistical models typically of class \dQuote{lm} or \dQuote{glm}.
#' 
#' @rdname bootstrap
#' @export
bootstrap.model <- function (x, ...)
  list(
    alpha = NULL,
    beta = coef(x)
  )
