#' @encoding UTF-8
#' @title Resamples Data Using the Jackknife Method
#'
#' @description
#' This function is used for estimating standard errors when the distribution is not know.
#'
#' @param x a vector
#' @param p estimation of parameter
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
#' jackknife(x,mean)
#'
#' @export
`jackknife` <-function (x,p)
{
  n=length(x)
  jk=rep(NA,n)

  for (i in 1:n)
  {
    jk[i]=p(x[-i])
    jkest=mean(jk)
    jkvar=(n-1)/n*sum((jk-jkest)^2)
    jkbias=(n-1)*(jkest-p(x))
    jkbiascorr=n*p(x)-(n-1)*jkest
  }
  list(est=p(x), jkest=jkest, jkvar=jkvar, jkbias=jkbias, jkbiascorr=jkbiascorr)
}
NULL
