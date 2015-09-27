#' @title Bonett-Seier test of Geary's kurtosis
#'
#' @description Performs the Bonett-Seier test of Geary's measure of kurtosis for normally distributed data.
#' @param x A numeric vector of data values.
#' @param alternative A character string specifying the alternative hypothesis,
#'  must be one of '"two.sided"' (default), '"greater"' or '"less"'.
#'   You can specify just the initial letter
#'
#'   @details  Under the hypothesis of normality, data should have Geary's
#'    kurtosis equal to \code{sqrt(2/pi)} (0.7979). This test has such null
#'     hypothesis and is useful to detect a significant difference of Geary's
#'      kurtosis in normally distributed data.
#'  @references
#'  Bonett, D.G., Seier, E. (2002) A test of normality with high uniform power. Computational Statistics and Data Analysis, 40, 435-445.
#' @importFrom stats complete.cases  pnorm
#' @export
#' @examples
#' set.seed(1234)
#' x = rnorm(1000)
#' geary(x)
#' bonett.seier(x)

`bonett.seier` <-
function (x, alternative=c("two.sided","less","greater"))
{
     DNAME <- deparse(substitute(x))
     x <- sort(x[stats::complete.cases(x)])
     n <- length(x)
s <- match.arg(alternative)
alter <- switch(s, two.sided=0, less=1, greater=2)
rho <- sqrt(sum((x-mean(x))^2)/n);
tau <- sum(abs(x-mean(x)))/n;
omega <- 13.29*(log(rho)-log(tau));
z <- sqrt(n+2)*(omega-3)/3.54;
     pval <- stats::pnorm(z, lower.tail = FALSE)
if (alter == 0) {
pval <- 2*pval
if (pval > 1) pval<-2-pval
alt <- "kurtosis is not equal to sqrt(2/pi)"
}
else if (alter == 1)
{
alt <- "kurtosis is greater than sqrt(2/pi)"
}
else
{
pval <- 1-pval
alt <- "kurtosis is lower than sqrt(2/pi)"
}
     RVAL <- list(statistic = c(tau = tau, z = z), alternative = alt,
p.value = pval, method = "Bonett-Seier test for Geary kurtosis",
         data.name = DNAME)
     class(RVAL) <- "htest"
     return(RVAL)
}

