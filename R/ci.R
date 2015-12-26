#' @encoding UTF-8
#' @title Confidence Intervals
#' @description Calculates the confidence intervals of a vector.
#' @keywords Univariate Stats
#' @param x a vector of data.
#' @param conf.level confidence level. Default is 0.95.
#' @param alpha confidence level. Default is 1-conf.level.
#' @param na.rm A logical value, default is \code{FALSE}
#' @param \dots Extra parameters.
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}.
#' @return
#' \item{CI lower}{Lower bound of interval.}
#' \item{Est. Mean}{Mean of data.}
#' \item{CI upper}{Upper bound of interval.}
#' \item{Std. Error}{Standard Error of the mean.}
#'
#' @docType methods
#' @rdname ci-methods
#' @examples
#'  x <- c(1, 2.3, 2, 3, 4, 8, 12, 43, -1,-4)
#'
#' ci(x, conf.level=.95)
#' @export
#' @aliases ci,numeric,numeric,numeric,logical,ci-method
`ci`<-setClass("ci", representation(x = "numeric", conf.level = "numeric",alpha = "numeric",na.rm="logical"))

setGeneric("ci", def=function(x, conf.level=0.95, alpha=1-conf.level,...){
  standardGeneric("ci")
})
NULL

#' @rdname ci-methods
setMethod(f="ci", definition=function(x, conf.level=0.95, alpha=1-conf.level,na.rm=FALSE,...){
  est <- mean(x, na.rm = na.rm);
  stderr <- stats::sd(x, na.rm=na.rm)/sqrt(nobs(x));
  ci.low <- est + stats::qt(alpha/2,nobs(x)-1)*stderr;
  ci.high <- est - stats::qt(alpha/2,nobs(x)-1)*stderr;
  retval <- c(
    "CI Lower"=ci.low,
    "Est. Mean"=est,
    "CI Upper"=ci.high,
    "Std. Error"=stderr
  );
  retval;
})
NULL






