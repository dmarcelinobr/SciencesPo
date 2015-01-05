#' @encoding UTF-8
#' @title Compute the Skewness
#' 
#' @description The function provides three features to perform a skewness test, see details below.
#'  
#'  @param x a numeric vector containing the values whose skewness is to be computed.
#'  @param na.rm a logical value for \code{na.rm}, default is \code{na.rm=TRUE}.
#'  @param type an integer between 1 and 3 selecting one of the algorithms for computing skewness detailed below.
#'  
#'  @details The skewness is a measure of symmetry distribution. Intuitively, negative skewness (g_1 < 0) indicates that the mean of the data distribution is less than the median, and the data distribution is left-skewed. Positive skewness (g_1 > 0) indicates that the mean of the data values is larger than the median, and the data distribution is right-skewed. Values of g_1 near zero indicate a symmetric distribution. The skewness function will ignore missing values in \sQuote{x} for its computation purpose. There are several methods to compute skewness, Joanes and Gill (1998) discuss three of the most traditional methods. According to them, \bold{type 3} performs better in non-normal population distribution, whereas in normal-like population distribution type 2 fits better the data. Such difference between the two formulae tend to disappear in large samples.
#'  \bold{Type 1:} g_1 = m_3/m_2^(3/2). 
#'
#' \bold{Type 2:} G_1 = g_1*sqrt(n(n-1))/(n-2). 
#'
#' \bold{Type 3:} b_1 = m_3/s^3 = g_1 ((n-1)/n)^(3/2).
#'  
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' 
#' @return An object of the same type as \code{x}
#' 
#' @references Joanes, D. N. and C. A. Gill. (1998) Comparing measures of sample skewness and kurtosis. \emph{The Statistician,} \bold{47,} 183--189.
#' 
#' 
#' @examples
#' w <-sample(4,10, TRUE)
#' x <- sample(10, 1000, replace=TRUE, prob=w)
#' skewness(x, type = 1)
#' skewness(x)
#' skewness(x, type = 3)
#' 
#' @export
#' 
skewness <-
function (x, na.rm = TRUE, type = 2) 
    {
        if (length(dim(x)) == 0) {
            if (na.rm) {
                x <- x[!is.na(x)]
            }
            stdev <- sd(x, na.rm = na.rm)
            mu <- mean(x)
            n <- length(x[!is.na(x)])
            switch(type, {
                skewer <- sqrt(n) * (sum((x - mu)^3, na.rm = na.rm)/(sum((x - 
                                                                              mu)^2, na.rm = na.rm)^(3/2)))
            }, {
                skewer <- n * sqrt(n - 1) * (sum((x - mu)^3, na.rm = na.rm)/((n - 
                                                                                  2) * sum((x - mu)^2, na.rm = na.rm)^(3/2)))
            }, {
                skewer <- sum((x - mu)^3)/(n * sd(x)^3)
            })
        }
        else {
            skewer <- rep(NA, dim(x)[2])
            if (is.matrix(x)) {
                mu <- colMeans(x, na.rm = na.rm)
            }
            else {
                mu <- apply(x, 2, mean, na.rm = na.rm)
            }
            stdev <- apply(x, 2, sd, na.rm = na.rm)
            for (i in 1:dim(x)[2]) {
                n <- length(x[!is.na(x[, i]), i])
                switch(type, {
                    skewer[i] <- sqrt(n) * (sum((x[, i] - mu[i])^3, 
                                                na.rm = na.rm)/(sum((x[, i] - mu[i])^2, na.rm = na.rm)^(3/2)))
                }, {
                    skewer[i] <- n * sqrt(n - 1) * (sum((x[, i] - 
                                                             mu[i])^3, na.rm = na.rm)/((n - 2) * sum((x[, 
                                                                                                        i] - mu[i])^2, na.rm = na.rm)^(3/2)))
                }, {
                    skewer[i] <- sum((x[, i] - mu[i])^3, na.rm = na.rm)/(n * 
                                                                             stdev[i]^3)
                })
            }
        }
        return(skewer)
    }
