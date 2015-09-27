#' @title Jarque-Bera test for normality
#'
#' @description This function performs the Jarque-Bera test on the given data sample to determine if the data are sample drawn from a normal population.
#' @param x  A numeric vector of data.
#' @details The Jarque-Bera statistic is chi-square distributed with two degrees of freedom. Under the hypothesis of normality, data should be symmetrical (i.e. skewness should be equal to zero) and have skewness chose to three.
#'
#' @references
#' Jarque, C. M., Bera, A. K. (1980) Efficient test for normality, homoscedasticity and serial independence of residuals,
#' Economic Letters, Vol. 6 Issue 3, 255-259.
#'
#' @examples
#' set.seed(1234)
#' x <- rnorm(1000)
#' jarque.bera(x)
#' @export
jarque.bera <- function(x)
{
    if ( !is.vector( x ) )
        stop( "argument x is not a vector" )
    if ( !is.numeric( x ) )
        stop( "argument x is not numeric" )
    DNAME <- deparse( substitute( x ) )
    n <- length(x)
    ALTERNATIVE <- "greater"
    METHOD <- "Jarque-Bera Normality Test"
    K <- kurtosis( x )
    S <- skewness( x )
    JB  <- ( n / 6 ) * ( S^2 + 0.25 * ( ( K - 3 )^2 ) )
    pval <- 1 - pchisq( JB, df=2 )
    JBVAL <- list( statistic=c(JB=JB), p.value=pval, alternative=ALTERNATIVE, method=METHOD,
                   data.name=DNAME )
    class( JBVAL ) <- "htest"
    return( JBVAL )
}
