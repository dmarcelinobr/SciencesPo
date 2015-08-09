#' @title D'Agostino test of skewness
#' @description Performs D'Agostino test for skewness in normally distributed data.
#' @param x A numeric vector of data values.
#' @param alternative A character string specifying the alternative hypothesis,
#' must be one of '"two.sided"' (default), '"greater"' or '"less"'.
#'  You can specify just the initial letter.
#'  @details Under the hypothesis of normality, data should be symmetrical (i.e. skewness should be equal to zero).
#'  This test has such null hypothesis and is useful to detect a significant skewness in normally distributed data.
#' @references
#' D'Agostino, R.B. (1970). Transformation to Normality of the Null Distribution of G1. Biometrika, 57, 3, 679-681.
#' @export
#' @examples
#' set.seed(1234)
#' x = rnorm(1000)
#' skewness(x)
#' agostino(x)
#'
`agostino` <-
function (x, alternative=c("two.sided","less","greater"))
{
     DNAME <- deparse(substitute(x))
     x <- sort(x[complete.cases(x)])
     n <- length(x)
s <- match.arg(alternative)
alter <- switch(s, two.sided=0, less=1, greater=2)
     if ((n < 8 || n > 46340))
         stop("sample size must be between 8 and 46340")
s3 <- (sum((x-mean(x))^3)/n)/(sum((x-mean(x))^2)/n)^(3/2)
y <- s3*sqrt((n+1)*(n+3)/(6*(n-2)))
b2 <- 3*(n*n+27*n-70)*(n+1)*(n+3)/((n-2)*(n+5)*(n+7)*(n+9))
w <- sqrt(-1+sqrt(2*(b2-1)));
d <- 1/sqrt(log(w));
a <- sqrt(2/(w*w-1));
z <- d*log(y/a+sqrt((y/a)^2+1));
     pval <- pnorm(z, lower.tail = FALSE)
if (alter == 0) {
pval <- 2*pval
if (pval > 1) pval<-2-pval
alt <- "data have a skewness"
}
else if (alter == 1)
{
alt <- "data have positive skewness"
}
else
{
pval <- 1-pval
alt <- "data have negative skewness"
}
     RVAL <- list(statistic = c(skew = s3, z = z), p.value = pval,
alternative = alt, method = "D'Agostino skewness test",
         data.name = DNAME)
     class(RVAL) <- "htest"
     return(RVAL)
}

