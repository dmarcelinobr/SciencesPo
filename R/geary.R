#' @encoding UTF-8
#' @title Geary's test for normality
#' @description This function computes an estimator of Geary’s measure of kurtosis.
#' @param x the numeric vector.
#' @param na.rm A logical for NA values.
#' @details Null hypothesis is that the data obeys to normal distribution and that data should have kurtosis equal to 3.
#' @return statistic The Geary's test statistic G.
#' @return p.value The significant probability of the null-hypothesis testing.
#' @author Daniel Marcelino \email{dmarcelino@@live.com}
#' @examples
#' set.seed(1234)
#' x = rnorm(1000)
#' geary(x)
#'
#' y <- c(2,3,4,5,6,7)
#' geary(20:50)
#' @export
`geary` <- function(x, na.rm=TRUE) {
  if (any(i.na <- is.na(x))) {
    if(na.rm)
      x <- x[!i.na]
    else return(NA)
  }
  DNAME <- deparse(substitute(x))
  mu <- mean(x)
  n <- length(x)
  kurt <- n*sum( (x-mean(x))^4 )/(sum( (x-mean(x))^2 )^2);
  G <- sum(abs(x-mu))/sqrt(n*sum((x-mu)^2))
  pval <- (1-stats::pnorm((G-sqrt(2/pi))/sqrt(1-3/pi)*sqrt(n)))*2


  RVAL <- list(statistic = c(kurt = kurt, G = G), p.value = pval,
              method = "Geary's test for normality",
               data.name = DNAME)
  class(RVAL) <- "htest"
  return(RVAL)
}
NULL
