#' @title Bartels Rank Test of Randomness
#'
#' @description Performs Bartels rank test of randomness. The default method for testing the null hypothesis of randomness is \code{two.sided}. By using the alternative \code{left.sided}, the null hypothesis is tested against a trend. By using the alternative \code{right.sided}, the null hypothesis of randomness is tested against a systematic oscillation in the observed data.
#'
#' @param x a numeric vector of data values.
#' @param alternative a character string for hypothesis testing method;
#' must be one of \code{two.sided} (default), \code{left.sided} or \code{right.sided}.
#' @param pvalue A method for asymptotic aproximation used to compute the p-value.
#' @details Missing values are by default removed.
#'
#' The RVN test statistic is
#' \deqn{RVN=\frac{\sum_{i=1}^{n-1}(R_i-R_{i+1})^2}{\sum_{i=1}^{n}\left(R_i-(n+1)/2\right)^2}}{RVN=\sum(R_i-R_{i+1})^2 / \sum(R_i-(n+1)/2)^2}
#' where \eqn{R_i=rank(X_i), i=1,\dots, n}{R_i=rank(X_i), i=1,...,n}. It is known that \eqn{(RVN-2)/\sigma} is asymptotically standard normal, where \eqn{\sigma^2=\frac{4(n-2)(5n^2-2n-9)}{5n(n+1)(n-1)^2}}{\sigma^2=[4(n-2)(5n^2-2n-9)]/[5n(n+1)(n-1)^2]}.
#'
#' @return
#' \item{statistic}{ The value of the RVN statistic test and the theoretical mean value and variance of the RVN statistic test.}
#' \item{n}{ the sample size, after the remotion of consecutive duplicate values.}
#' \item{p.value}{the asymptotic p-value.}
#' \item{method}{a character string indicating the test performed.}
#' \item{data.name}{a character string giving the name of the data.}
#' \item{alternative}{a character string describing the alternative.}
#'
#' @keywords Tests
#' @references
#' Bartels, R. (1982). The Rank Version of von Neumann's Ratio Test for Randomness, \emph{Journal of the American Statistical Association}, \bold{77}(377), 40-46.
#'
#' Gibbons, J.D. and Chakraborti, S. (2003). \emph{Nonparametric Statistical Inference}, 4th ed. (pp. 97-98). URL: \url{http://books.google.pt/books?id=dPhtioXwI9cC&lpg=PA97&ots=ZGaQCmuEUq}
#'
#' @examples
#' # Example 5.1 in Gibbons and Chakraborti (2003), p.98.
#' # Annual data on total number of tourists to the United States for 1970-1982.
#'  years <- 1970:1982
#'  tourists <- c(12362, 12739, 13057, 13955, 14123,  15698, 17523,
#'  18610, 19842, 20310, 22500, 23080, 21916)
#'
#'  # See it graphically
#'  qplot(factor(years), tourists)+ geom_point()
#'
#' # Test the null against a trend
#'  Bartels(tourists, alternative="left.sided", pvalue="beta")
#'
#' @export
`Bartels` <-
  function(x,
           alternative = "two.sided",
           pvalue = "normal")
    UseMethod("Bartels")


#' @rdname Bartels
#' @export
`Bartels.default` <-
  function(x,
           alternative = "two.sided",
           pvalue = "normal") {
    dname <- deparse(substitute(x))
    # Remove NAs
    x <- na.omit(x)
    stopifnot(is.numeric(x))
    n <- length(x)
    if (alternative == "t") {
      alternative <- "two.sided"
    }
    if (alternative == "l") {
      alternative <- "left.sided"
    }
    if (alternative == "r") {
      alternative <- "right.sided"
    }
    if (alternative != "two.sided" &
        alternative != "left.sided" & alternative != "right.sided")
    {
      stop("must give a valid alternative")
    }
    if (n < 10) {
      stop("sample size must be greater than 9")
    }
    # unique
    rk <- rank(x)
    d <- diff(rk)
    #d.rank <- n*(n^2-1)/12
    d.rank <- sum(rk ^ 2) - n * (mean(rk) ^ 2)
    RVN <- sum(d ^ 2) / d.rank
    mu <- 2
    vr <- (4 * (n - 2) * (5 * n ^ 2 - 2 * n - 9)) / (5 * n * (n + 1) * (n - 1) ^ 2)
    # Computes the p-value
    if (pvalue == "auto") {
      pvalue <- ifelse(n <= 100, "beta", "normal")
    }
    if (pvalue == "beta") {
      btp <- (5 * n * (n + 1) * (n - 1) ^ 2) / (2 * (n - 2) * (5 * n ^ 2 - 2 * n - 9)) - 1 / 2
      pv0 <- stats::pbeta(RVN / 4, shape1 = btp, shape2 = btp)
    }
    if (pvalue == "normal") {
      pv0 <- stats::pnorm((RVN - mu) / sqrt(vr))
    }
    if (alternative == "two.sided") {
      pv <- 2 * min(pv0, 1 - pv0)
      alternative <- "nonrandomness"
    }
    if (alternative == "left.sided") {
      pv <- pv0
      alternative <- "trend"
    }
    if (alternative == "right.sided") {
      pv <- 1 - pv0
      alternative <- "systematic oscillation"
    }
    test <- (RVN - mu) / sqrt(vr)
    rval <-
      list(
        statistic = c(statistic = test),
        nm = sum(d ^ 2),
        rvn = RVN,
        mu = mu,
        var = vr,
        p.value = pv,
        method = "Bartels Ratio Test",
        data.name = dname,
        parameter = c(n = n),
        n = n,
        alternative = alternative
      )
    class(rval) <- "htest"
    return(rval)
  } ### end -- Bartels function
NULL




#' @encoding UTF-8
#' @title James-Stein Shrunken Estimates
#'
#' @description Computes James-Stein shrunken estimates of cell means given
#' a response variable (which may be binary) and a grouping indicator.
#'
#' @references
#' Efron, Bradley and Morris, Carl (1977) ``Stein's Paradox in Statistics.'' \emph{Scientific American} Vol. 236 (5): 119-127.
#'
#' James, W., & Stein, C. (1961, June).
#' \href{http://projecteuclid.org/euclid.bsmsp/1200512173}{Estimation with
#' quadratic loss}. \emph{In Proceedings of the fourth Berkeley symposium on
#' mathematical statistics and probability} (Vol. 1, No. 1961, pp. 361-379).
#'
#' @param x a vector, matrix, or array of numerics; the data.
#' @param tau2 a positive numeric. The variance, assumed known and defaults to 1.
#' # @param k the grouping factor.
#' @param \dots extra parameters typically ignored.
#'
#' @details Missing values are by default removed.
#' @keywords Tests
#'
#'
#' @export
`JamesStein` <- function(x, tau2 = 1) UseMethod("JamesStein")
#'
#' @rdname JamesStein
#' @export
JamesStein <- function(x, tau2 = 1) {
  p <- dim(x)
  ## returns Stein's estimator
  sum_x <- sum(x ^ 2)
  est <- (1 - (prod(p) - 2) * tau2 / sum_x) * x
  sure_est <- prod(p) * tau2 - (prod(p) - 2) ^ 2 * tau2 ^ 2 / sum_x
  return(list(est = est, sure_est = sure_est))
}### end -- james.stein function
NULL



