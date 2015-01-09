#' @encoding UTF-8
#' @title Pearson's Coefficient of Variation 
#'
#' @description Compute the absolute \bold{coefficient of variation} \bold{cv} as proposed by Karl Pearson, which is given by the division of standard deviation by the mean. The CV reflects a normalized measure of the dispersion of a given probability distribution. Conversely, distributions with \deqn{cv < 1} are considered \dQuote{low-variance}, while those with \deqn{cv > 1} \dQuote{high-variance}.
#' 
#' @param x A numeric vector.
#'
#' @details \code{sd(x)/mean(x) = cv}, which is the inverse of signal-to-noise ratio.
#'
#' @return The coefficient of variation.
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#'
#' @keywords The Basics
#' @keywords Descriptive Stats
#'
#' @examples
#'
#' myvar <- sample(100) 
#' variation(myvar)
#'
#' @export
variation <-  
function(x){ 
  sd(x)/mean(x)
}
NULL





#' @encoding UTF-8
#' @title Compute the Kurtosis
#' 
#' @description Return the kurtosis test for object x. For vectors, kurtosis(x) is the kurtosis of the elements in the vector x. For matrices kurtosis(x) returns the sample kurtosis for each column of x. For N-dimensional arrays, kurtosis operates along the first nonsingleton dimension of x.Returns the kurtosis test for object x. For vectors, kurtosis(x) is the kurtosis of the elements in the vector x. For matrices kurtosis(x) returns the sample kurtosis for each column of x. For N-dimensional arrays, kurtosis operates along the first nonsingleton dimension of x.
#' 
#' @param x a numeric vector
#' @param na.rm a logical value for \code{na.rm}, default is \code{na.rm=FALSE}.
#' @param type an integer between 1 and 3 selecting one of the algorithms for computing kurtosis detailed below
#' 
#' @details In a similar way of skewness, kurtosis measures the peakedness of a data distribution. A distribution with zero kurtosis has a shape as the normal curve. Such type of kurtosis is called mesokurtic, or mesokurtotic. A positive kurtosis has a curve more peaked about the mean and the its shape is narrower than the normal curve. Such type is called leptokurtic, or leptokurtotic. Finally, a distribution with negative kurtosis has a curve less peaked about the mean and the its shape is flatter than the normal curve. Such type is called platykurtic, or platykurtotic. To be consistent with classical use of kurtosis in political science analyses, the default \bold{type} is the same equation used in SPSS and SAS, which is the bias-corrected formula: \bold{Type 2:} G_2 = ((n + 1) g_2+6) * (n-1)/(n-2)(n-3). When you set type to 1, the following equation applies: \bold{Type 1:} g_2 = m_4/m_2^2-3. When you set type to 3, the following equation applies: \bold{Type 3:} b_2 = m_4/s^4-3 = (g_2+3)(1-1/n)^2-3. You must have at least 4 observations in your vector to apply this function.
#' 
#' @return An object of the same type as \code{x}.
#' 
#' @references Balanda, K. P. and H. L. MacGillivray. (1988) Kurtosis: A Critical Review. \emph{The American Statistician,} \bold{42(2), pp. 111--119.}
#' 
#' @note \bold{Skewness} and \bold{Kurtosis} are functions to measure the third and fourth \bold{central moment} of a data distribution. 
#' 
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' 
#' @examples
#' 
#' w<-sample(4,10, TRUE)
#' x <- sample(10, 1000, replace=TRUE, prob=w)
#'
#' kurtosis(x, type=2)
#'
#' kurtosis(x, type=3)
#' 
#' 
#' @export
kurtosis <-
  function (x, na.rm = FALSE, type = 2) 
  {
    if (any(i.na <- is.na(x))) {
      if (na.rm) 
        x <- x[!i.na]
      else return(NA)
    }
    if (!(type %in% (1:3))) 
      stop("Your argument for 'type' is not valid.")
    n <- length(x)
    dev <- (x - mean(x))
    r <- (n * sum(dev^4)/(sum(dev^2)^2))
    y <- if (type == 1) 
      r - 3
    else if (type == 2) {
      if (n < 4) 
        stop("You need at least 4 complete observations.")
      ((n + 1) * (r - 3) + 6) * (n - 1)/((n - 2) * (n - 3))
    }
    else r * (1 - 1/n)^2 - 3
    y
  }
NULL



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
NULL




#' @title Weighted Variance
#'
#'@description Weighted Variance Formula
#'
#'@param x the varaible 
#'@param w the variance
#'
weighted.var <- function(x, w){
  return(sum(w * (x - weighted.mean(x,w))^2)/((length(x)-1)*mean(w)))
} 
NULL




#' @encoding UTF-8
#' @title Winsorized Mean 
#' 
#' @description Compute the winsorized mean, which consists of recoding the top k values in a vector.
#' 
#' @param x The vector to be winsorized
#' @param k An integer for the quantity of outlier elements that to be replaced in the calculation process
#' @param na.rm a logical value for \code{na.rm}, default is \code{na.rm=TRUE}.
#'
#' @details Winsorizing a vector will produce different results than trimming it. While by trimming a vector causes extreme values to be discarded, by winsorizing it in the other hand, causes extreme values to be replaced by certain percentiles.
#' 
#' @return An object of the same type as \code{x}
#'
#' @references  Dixon, W. J., and Yuen, K. K. (1999) Trimming and winsorization: A review. \emph{The American Statistician,} \bold{53(3),} 267--269.
#' @references  Dixon, W. J., and Yuen, K. K. (1960) Simplified Estimation from Censored Normal Samples, \emph{The Annals of Mathematical Statistics,} \bold{31,} 385--391.
#'  @references  Wilcox, R. R. (2012) \emph{Introduction to robust estimation and hypothesis testing.} Academic Press, 30-32. Statistics Canada (2010) \emph{Survey Methods and Practices.}
#'  
#'  @note One may want to winsorize estimators, however, winsorization tends to be used for one-variable situations.
#'  
#'  @author Daniel Marcelino, \email{dmarcelino@@live.com}
#'  
#'  
#'  @examples  
#'  x <- rnorm(100) 
#'  winsorize(x)
#'  # see this function in context.
#'
#' @keywords Descriptive
#' 
#' @export
#'
winsorize <-
  function (x, k = 1, na.rm=TRUE) {
    if (any(is.na <- is.na(x))) {
      if (na.rm) 
        x <- x[!is.na]
      else return(NA)
    }
    n <- length(x)
    if (!(k %in% (0:n))) 
      stop("'k' should be > 0 and less than half the number of non-missing observations.")
    else {
      x <- sort(x)
      x[1:k] <- x[k+1] # Here I solve the lower values
      x[(n-k+1):n] <- x[n-k] #Then I go over the higher ones
      return(mean(x))
    }
  }
NULL





#' @encoding UTF-8
#' @title Calculate the Standard Error
#'
#' @description Compute the standard errors of a numeric vector
#' 
#' @aliases std.error
#' @param x  A vector of class numeric or integer
#' @param na.rm a logical value for \code{na.rm}, default is \code{na.rm=TRUE}.
#' 
#' @details The standard error of the mean (SEM) (\emph{assuming statistical independence of the values in the sample}) is estimated by taking the standard deviation of the population sample, divided by the square root of the sample size: \deqn{se = \frac{{s}}{{\sqrt{n}}}}
#' 
#' @references Kenney, J. F. and Keeping, E. S. (1951) Standard Error of the Mean. In \emph{Mathematics of Statistics,} Pt. 2, 2nd ed. Princeton, NJ: Van Nostrand, pp. 110 and 132--133.
#' 
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#'
#' @examples
#' x <- rnorm(100); 
#' se(x)
#'
#' @export 
se <-
  function (x, na.rm = TRUE) 
  {
    valid <- function(x) return(sum(!is.na(x)))
    dim <- dim(x)
    if (is.null(dim)) {
      sd <- sd(x, na.rm = na.rm)
      n.valid <- valid(x)
    }
    else {
      if (is.data.frame(x)) {
        n.valid <- unlist(sapply(x, valid))
        sd <- unlist(sapply(x, sd, na.rm = na.rm))
      }
      else {
        n.valid <- unlist(apply(x, 2, valid))
        sd <- unlist(apply(x, 2, sd, na.rm = na.rm))
      }
    }
    return(sd/sqrt(n.valid))
  }
NULL




#' @encoding UTF-8
#' @title Calculate the Log Likelihood of a Normal Distribution
#' 
#' @description 
#' Find the log likelihood of a normal distribution.
#' 
#' @param x data.
#' @param mu estimated mean.
#' @param var estimated variance.
#' 
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' 
#' @return ll logliklihood of the distribution
#' 
#' @examples
#' x = rnorm(100, 3, 7)
#' loglik(x,3,7)
#' 
#' @export 

loglik<-function(x=data, mu, var)
{
  n=length(x)
  ll = -n/2* log(2*pi*var) - .5/var*sum((mu-x)^2)
  
  -ll
}
NULL





#' @encoding UTF-8
#' @title Cramer's V for a contingency table
#'
#' @description Produce the Cramér V / Phi test using two vectors with more than 2 levels.
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#'  
#' @param x one vector
#' @param y the other vector
#' 
#' @return The table's Cramer's V.
#' 
#' @details Compute Cramer's V for a resulting table with more than 2x2 fields.
#' 
#' @examples
#'  x =sample(1:2, 30, TRUE); 
#'  y= sample(1:3, 30, TRUE)
#'  cramer(x, y)
#'
#' @export
cramer <- function(x, y) {
  chisq_test <- function (x, y) {
    O <- table(x, y)
    n <- sum(O)
    
    E <- outer(rowSums(O), colSums(O), "*")/n
    
    sum((abs(O - E))^2 / E)
  }
  chi <- chisq_test(x, y)
  
  ulength_x <- length(unique(x))
  ulength_y <- length(unique(y))
  # Cramér V / Phi:
  sqrt(chi / (length(x) * (min(ulength_x, ulength_y) - 1)))
}
NULL

 # Cramer's V for a contingency table
# Compute Cramer's V for a table with more than 2x2 fields.
 # @param tab a table or ftable object. Tables of class xtabs and other will be coerced to ftable internally.
 # cramer <-
 # function (tab) 
 # {
 #  if (class(tab) != "ftable") 
 #    tab <- ftable(tab)
 #  phi <- sjs.phi(tab)
 #  cramer <- sqrt(phi^2/min(dim(tab) - 1))
 #  return(cramer)
 # }

 #x <- vcd::Arthritis$Improved
 #y <- vcd::Arthritis$Treatment
 #correct <- vcd::assocstats(table(x, y))$cramer
 #correct
 # is_ok <- function(x) stopifnot(all.equal(x, correct))
  # is_ok(cramer(x, y))
 #
 #microbenchmark(
#  cramer1(x, y),
#  cramer(x, y),
 #  cramer_c(x, y)
 #)
 # cramer_c <- compiler::cmpfun(cramer)





#' @encoding UTF-8
#' @title  Calculate the Mode
#' 
#' @description Estimates the mode for a vector
#' 
#' @param x A data vector
#' @param na.rm A logical value, default is \code{FALSE}
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' 
#' @examples 
#' myvar <-c(1,1,2,2,3,3,4,4,5, NA)
#' mode(myvar)
#' 
#' mode(myvar, FALSE)
#' 
#' @export
mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = subset(x, !is.na(x))
  }
  y <- as.factor(x)
  freq <- summary(y)
  mode <- names(freq)[freq[names(freq)] == max(freq)]
  return(as.numeric(mode) )
}
NULL




#' @encoding UTF-8
#' @title Calculate the Sample Covariance
#' 
#' @description Computes the sample covariance between two vectors. The Covariance provides a measure of the strength of the correlation between two or more sets of random variates. The covariance for two random variates \code{x} and \code{y}, each with sample size \code{n}, is defined by the expectation value variables \verb{cov(x, y) = (x - \mu_x)(y - \mu_y)}. For uncorrelated variables, \code{cov(x, y) = 0}. 
#' 
#' @param x One of two vectors whose sample covariance is to be calculated.
#' @param y The other vector.
#' @param verbose If \code{TRUE}, prints sample covariance; if not, not. Default is \code{verbose = TRUE}.
#' 
#' @return The sample covariance between x and y.
#' 
#' @details x and y must have the same length, greater than one  with no missing values.
#' 
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @references Based on the Google's R Guide Style.
#' @examples
#' # Some random data:
#' df = data.frame(id=1:20, x=rnorm(20, mean=2, sd=.5), 
#'  y=rnorm(20, mean=5, sd=2) )
#'   sampleCovariance(df$x, df$y)
#'
#' @export
sampleCovariance <- function(x, y, verbose = TRUE) {
  n <- length(x)
  # Error handling
  if (n <= 1 || n != length(y)) {
    stop("Arguments x and y have different lengths: ",
         length(x), " and ", length(y), ".")
  }
  if (TRUE %in% is.na(x) || TRUE %in% is.na(y)) {
    stop(" Arguments x and y must not have missing values.")
  }
  covariance <- var(x, y)
  if (verbose)
    cat("Covariance = ", round(covariance, 4), ".\n", sep = "")
  return(covariance)
}
NULL
