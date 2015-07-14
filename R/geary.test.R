









#' @encoding UTF-8
#' @title Geary's test for normality
#' @description Geary's test for normality. Null hypothesis is that the data obeys to normal distribution.
#' @param x the numeric vector.
#'
#' @return statistic The Geary's test statistic G
#' @return p.value The significant probability of the null-hypothesis testing.
#' @author Daniel Marcelino \email{dmarcelino@@live.com}
#' @examples
#' s <-sample(100, 20)
#' geary.test(s)
#' geary.test(rnorm(100))
#' @export
`geary.test` <- function(x) {
  mu <- mean(x)
  n <- length(x)
  G <- sum(abs(x-mu))/sqrt(n*sum((x-mu)^2))
  p <- (1-stats::pnorm((G-sqrt(2/pi))/sqrt(1-3/pi)*sqrt(n)))*2
  cat("Geary's test for normality: G=",G," / p=",p,"\n")
}
NULL
