#' @encoding UTF-8
#' @title Confidence Intervals
#' @description Calculates the confidence intervals for a vector of data values.
#' @keywords Exploratory
#' @param x A vector of data values.
#' @param conf.level The confidence level. Default is \code{0.95}.
#' @param alpha The significance level. Default is \code{1-conf.level}. If alpha equals 0.05, then your confidence level is 0.95.
#' @param na.rm A logical value, default is \code{FALSE}
#' @param \dots Extra parameters.
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}.
#' @return
#' \item{CI lower}{Lower bound of interval.}
#' \item{Est. Mean}{Mean of data.}
#' \item{CI upper}{Upper bound of interval.}
#' \item{Std. Error}{Standard Error of the mean.}
#'
#' @examples
#' x <- c(1, 2.3, 2, 3, 4, 8, 12, 43, -1,-4)
#'
#' ci(x, conf.level=.90)
#'
#' @docType methods
#' @rdname ci-methods
#' @export
`ci`<-setClass("ci", representation(x = "numeric", conf.level = "numeric",alpha = "numeric",na.rm="logical"))


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






