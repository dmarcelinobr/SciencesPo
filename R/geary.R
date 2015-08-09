#' @encoding UTF-8
#' @title Geary's test for normality
#' @description Geary's test for normality. Null hypothesis is that the data obeys to normal distribution.
#' @param x the numeric vector.
#'
#' @return statistic The Geary's test statistic G
#' @return p.value The significant probability of the null-hypothesis testing.
#' @author Daniel Marcelino \email{dmarcelino@@live.com}
#' @examples
#' set.seed(1234)
#' x = rnorm(1000)
#' skewness(x)
#' geary(x)
#' @export
`geary` <- function(x) {
  mu <- mean(x)
  n <- length(x)
  G <- sum(abs(x-mu))/sqrt(n*sum((x-mu)^2))
  p <- (1-stats::pnorm((G-sqrt(2/pi))/sqrt(1-3/pi)*sqrt(n)))*2
  cat("Geary's test for normality: G=",G," / p=",p,"\n")
}
NULL
