#' @encoding UTF-8
#' @title Resamples Data Using The Jackknife Method
#'
#' @description
#' This function is mainly used to estimate statistics (standard errors), when the distribution is not know.
#'
#' @param x A vector
#' @param FUN a function name to estimate, i.e., 'mean', 'sd', 'var', 'cov', etc.
#' @param \dots further arguments passed to or used by other methods.
#'
#' @return est orignial estimation of parameter
#' @return jkest jackknife estimation of parameter
#' @return jkvar jackknife estimation of variance
#' @return jkbias jackknife estimate of biasness of parameter
#' @return jkbiascorr bias corrected parameter estimate
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#'
#' @examples
#' x = runif(10, 0, 1)
#' mean(x)
#' Jackknife(x, mean)
#'
#' @export
`Jackknife` <- function(x, FUN = mean, ...) UseMethod("Jackknife")

#' @rdname Jackknife
#' @export
`Jackknife.default` <-function (x, FUN = mean, ...)
{
	n=length(x)
	jk=rep(NA,n)
	est = match.fun(FUN)(x)

	for (i in 1:n)
	{
		jk[i]=match.fun(FUN)(x[-i])
		jkest=mean(jk)
		jkvar=(n-1)/n*sum((jk-jkest)^2)
		jkbias=(n-1)*(jkest-est)
		jkbiascorr=n*est-(n-1)*jkest
	}
	list(est=est, jkest=jkest, jkvar=jkvar, jkbias=jkbias, jkbiascorr=jkbiascorr)
}
NULL
