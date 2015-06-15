#' @encoding UTF-8
#' @title Pearson's Coefficient of Variation
#'
#' @description Compute the absolute \bold{coefficient of variation} \bold{cv} as proposed by Karl Pearson, which is given by the division of standard deviation by the mean. The CV reflects a normalized measure of the dispersion of a given probability distribution. Conversely, distributions with \eqn{cv < 1} are considered \dQuote{low-variance}, while those with \eqn{cv > 1} \dQuote{high-variance}.
#'
#' @param x A numeric vector.
#' @param na.rm A logical value, default is \code{FALSE}
#' @details \eqn{\frac{sd(x)}{mean(x)} = cv}, which is the inverse of signal-to-noise ratio.
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
#' cv(myvar)
#' @rdname cv-methods
#' @rdname variation
#' @export
setGeneric("cv", function(x, na.rm = TRUE){
  standardGeneric("cv")
})
#' @rdname cv-methods
#' @aliases cv,ANY,logical,ANY-method
setMethod(f="cv", signature=c(x="ANY", na.rm="logical"), definition=function(x, na.rm = TRUE){
	sd <- sd(x, na.rm = na.rm)
	 mean <- mean(x, na.rm = na.rm)
    return(sd/mean)
  })
NULL



#' @encoding UTF-8
#' @title  Calculate the Mode
#'
#' @description Estimates the mode for a vector
#'
#' @param x A data vector
#' @param na.rm A logical value, default is \code{FALSE}
#' @note this function replaces the \code{base} function with the same name, but is does different thing. While \code{SciencesPo::mode} intuitively calculates the \dQuote{mode}, \code{base::mode} prints the \dQuote{class} of an object.
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
`mode` <- function(x, na.rm = FALSE) {
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
#' @title Variance Inflation Factor
#' @description Extracts Variance Inflation Factor from a model of class \dQuote{lm}
#' @param model a model object
#' @param \dots further arguments passed to or used by other methods.
#' @return A numeric value indicating the variance inflation in the model
#' #' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @keywords Models
#' @examples
#' data(mtcars)
#' m1 <- lm(mpg ~ qsec + hp, data=mtcars)
#' vif(m1)
#' @export
`vif` <-
  function(model, ...) {
    if (any(is.na(coef(model))))
      stop ("there are aliased coefficients in the model")
    v <- vcov(model)
    assign <- attributes(model.matrix(model))$assign
    if (names(coefficients(model)[1]) == "(Intercept)") {
      v <- v[-1, -1]
      assign <- assign[-1]
    }
    else warning("Vifs may not be sensible without intercept.")
    parameters <- labels(terms(model))
    n.parameters <- length(parameters)
    if (n.parameters < 2) stop("model contains fewer than 2 parameters")
    R <- cov2cor(v)
    detR <- det(R)
    result <- matrix(0, n.parameters, 3)
    rownames(result) <- parameters
    colnames(result) <- c("GVIF", "Df", "GVIF^(1/(2*Df))")
    for (parameters in 1:n.parameters) {
      subs <- which(assign == parameters)
      result[parameters, 1] <- det(as.matrix(R[subs, subs])) *
        det(as.matrix(R[-subs, -subs])) / detR
      result[parameters, 2] <- length(subs)
    }
    if (all(result[, 2] == 1)) result <- result[, 1]
    else result[, 3] <- result[, 1]^(1/(2 * result[, 2]))
    result
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
`kurtosis` <-
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
#'  @param type an integer between 1 and 3 for selecting the algorithms for computing the skewness, see details below.
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
`skewness` <-
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
`weighted.var` <- function(x, w){
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
#' set.seed(51)  # for reproducibility
#' x <- rnorm(50)
#' ## introduce outliers
#' x[1] <- x[1] * 10
#' # Compare to mean:
#'  mean(x)
#'  winsorize(x)
#' @keywords Descriptive
#'
#' @export
#'
`winsorize` <-
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
#' @title Calculates the Standard Error
#'
#' @description Compute the standard errors of a numeric vector
#'
#' @aliases std.error
#' @param x  a vector of class numeric or integer
#' @param na.rm a logical value for \code{na.rm}, default is \code{na.rm=TRUE}.
#' @param \dots some extra parameters.
#' @details The standard error of the mean (SEM) (\emph{assuming statistical independence of the values in the sample}) is estimated by taking the standard deviation of the population sample, divided by the square root of the sample size: \deqn{se = \frac{{s}}{{\sqrt{n}}}}
#'
#' @references Kenney, J. F. and Keeping, E. S. (1951) Standard Error of the Mean. In \emph{Mathematics of Statistics,} Pt. 2, 2nd ed. Princeton, NJ: Van Nostrand, pp. 110 and 132--133.
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @docType methods
#' @rdname se-methods
#'
#' @examples
#' x <- rnorm(100);
#' se(x)
#' @export
setGeneric("se", function(x, na.rm = TRUE){
  standardGeneric("se")
})

#' @rdname se-methods
#' @aliases se,ANY,logical,ANY-method
setMethod(f="se", signature=c(x="ANY", na.rm="logical"), definition=function(x, na.rm = TRUE){
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
})
NULL



#' @encoding UTF-8
#' @title Association tests for contingency tables
#' @description Calculates the Likelihood Ratio chi-Squared test, the Pearson chi-Squared test, the phi coefficient, the contingency coefficient and Cramer's V. Cramer V / Phi test using two vectors with more than 2 levels.
#'
#' @param x an \code{r x c} table.
#' @return The association statistics tests for \code{r x c} table.
#' @importFrom MASS loglm
#'
#' @examples
#'  x = sample(1:2, 30, TRUE);
#'  y = sample(1:3, 30, TRUE)
#'  a.test(table(x, y))
#'
#' @export
`a.test` <- function(x) {
  if (!is.matrix(x))
    stop("Function only defined for 2-way tables.")
  tab <- summary(loglm(~1 + 2, x))$tests
  phi <- sqrt(tab[2, 1]/sum(x))
  cont <- sqrt(phi^2/(1 + phi^2))
  cramer <- sqrt(phi^2/min(dim(x) - 1))
  structure(list(table = x, chisq_tests = tab, phi = phi, contingency = cont,cramer = cramer))
}
NULL




#' @encoding UTF-8
#' @title Stukel's test of the logistic link
#' @description The Stukel's test is an alternative to the goodness-of-fit test for logistic regression.
#'  It tests if significant change occurs in the model with the addition of new coefficients.
#'
#' @param object an object of class \code{glm}.
#' @param alternative add both \code{z1} and \code{z2} to model or just one of them.
#'
#' @details Two new covariates, z1 and z2 are generated such that \deqn{z1 = 0.5 \* logit^{2} * I(pi >= 0.5)}, \deqn{z2 = - 0.5 \* logit^{2} \* I(pi <= 0.5)}, where \deqn{I(arg) = 1} if arg is \code{TRUE} and \deqn{I(arg) = 1} if \code{FALSE}.
#' @note Adapted from program published by Brett Presnell's code available at the Florida University.
#'@references
#' Stukel, T.A. (1988) Generalized logistic models. \emph{Journal of the American Statistical Association} 83: 426-431.
#'@references
#' Hosmer, David W., et al (1997) A comparison of goodness-of-fit tests for the logistic regression model. \emph{Statistics in medicine} 16.9, 965-980.
#' @references
#'Allison, Paul (2014) \emph{Another Goodness-of-Fit Test for Logistic Regression}.
#' @export
`stukel` <- function(object, alternative = c("both", "alpha1", "alpha2")) {
  DNAME <- deparse(substitute(object))
  METHOD <- "Stukel's test of the logistic link"
  alternative <- match.arg(alternative)
  eta <- predict(object, type = "link")
  etasq <- 0.5 * eta * eta
  etapos <- eta > 0
  dv <- matrix(0, nrow = length(eta), ncol = 2)
  dv[etapos,1] <- etasq[etapos]
  dv[!etapos,2] <- - etasq[!etapos]
  colnames(dv) <- c("z1","z2")
  oinfo <- vcov(object)
  oX <- qr.X(object$qr)
  ImH <- - oX %*% oinfo %*% t(oX)
  diag(ImH) <- 1 + diag(ImH)
  wdv <- sqrt(object$weights) * dv
  qmat <- t(wdv) %*% ImH %*% wdv
  sc <- apply(dv * (object$weights * residuals(object, "working")), 2, sum)
  allstat <- c(sc * sc / diag(qmat), sc %*% solve(qmat) %*% sc)
  names(allstat) <- c("alpha1", "alpha2", "both")
  allpar <- c(1,1,2)
  names(allpar) <- names(allstat)
  allpval <- pchisq(allstat, allpar, lower.tail=FALSE)
  STATISTIC <- allstat[alternative]
  PARAMETER <- allpar[alternative]
  names(PARAMETER) <- "df"
  PVAL <- allpval[alternative]
  names(allpar) <- rep("df", 3)
  structure(list(statistic = STATISTIC,
                 parameter = PARAMETER,
                 p.value = PVAL,
                 alternative = alternative,
                 method = METHOD, data.name = DNAME,
                 allstat = allstat, allpar = allpar, allpval = allpval
  ),
  class = "htest")
}
NULL





#' @encoding UTF-8
#' @title Cumulative Logit
#'
#' @param y the dependent variable
#' @param adj adjustment constant
#' @author Daniel Marcelino \email{dmarcelino@@live.com}
#'@export
`cumulative.logit` <- function(y, adj = 0.5) {
  ncol <- dim(y)[2]
  y <- t(apply(y, 1, cumsum))
  log((y[,-ncol] + adj)/(y[,ncol] - y[,-ncol] + adj))
}
NULL



#' @encoding UTF-8
#' @title Adjusted Residuals
#'
#' @param object a model object of type \code{glm} or \code{lm}.
#' @param \dots further arguments passed to or used by other methods.
#' @author Daniel Marcelino \email{dmarcelino@@live.com}
#'@export
`adjusted.residuals` <- function(object, ...) {
  residuals(object, ...) / sqrt(1 - lm.influence(object)$hat)
}
NULL



#' @encoding UTF-8
#'@title Calculates the true median
#' @description Usually median for data with ties, the tied values are treated as exactly the same. For instance, taking a median of 3, 3, 4, 4, 4 will be 4. However, the values to be measured are usually rounded off, so that we can assume evenly distributed true values for tied values. For instance, the previous data can be treated as rounded values of 2.75, 3.25, 11/3, 4, 13/3. From this viewpoint, the true median of 3, 3, 4, 4, 4 could be 11/3 (=3.66...).
#' @param x a numeric vector.
#' @param h	width of measurement unit. Default is \code{h=1}.
#' @author Daniel Marcelino \email{dmarcelino@@live.com}
#'@examples
#' s <-c(3, 3, 4, 4, 4)
#' true.median(s)
#' median(s)
#' z <- c(2.75, 3.25, 11/3, 4, 13/3)
#' true.median(z)
#' median(z)
#' @export
`true.median` <- function(x,h=1) {
  YY <- rep(0,length(x))
  XX <- table(x)
  q <- length(XX)
  k <- 0
  for (i in 1:q) {
    L <- as.numeric(names(XX)[i])-h/2
    for (j in 1:XX[[i]]) {
      k <- k+1
      YY[k] <- L+h*(2*j-1)/(2*XX[[i]])
    }
  }
  median(YY)
}
NULL


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
p <- (1-pnorm((G-sqrt(2/pi))/sqrt(1-3/pi)*sqrt(n)))*2
cat("Geary's test for normality: G=",G," / p=",p,"\n")
}
NULL




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




#' @title Computes the region significance
#' @description Probing Regression Interactions
#' @param y the dependent variable.
#' @param x the independent variable.
#' @param z the moderator variable.
#' @param zval an specific value for the moderator (z).
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#'
#'@export
`region.significance` = function(y, x, z , zval){
  mod = lm(y~x + z + predictor:z)
  gam1.v = vcov(mod)["x","x"]
  gam3.v = vcov(mod)["x:z","x:z"]
  gam1gam3.cv = vcov(mod)["x","z"]
  df = length(y)-length(coef(mod))
  se.w1 = sqrt(gam1.v + 2*zval*gam1gam3.cv + zval^2*gam3.v)
  w1.hat = coef(mod)["x"]+coef(mod)["x:z"]*zval
  p.value = (1-pt(w1.hat/se.w1,df=df))*2
  w1.tab = cbind(w1.hat,se.w1,zval,p.value)
  rownames(w1.tab) = "Moderator"
  colnames(w1.tab) = c("Est","SE","Z","p-value")
  w1.tab
}
NULL




#' @title Johnson-Neyman Regression
#' @description Probing Regression Interactions
#' @param y the dependent variable.
#' @param x the independent variable.
#' @param z the moderator variable.
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#'
#'@export
`johnson.neyman` = function(y,x,z){
  mod = lm(y~x + z + x:z)
  gam1.v = vcov(mod)["x","x"]
  gam3.v = vcov(mod)["x:z","x:z"]
  gam1gam3.cv = vcov(mod)["x","z"]
  df = length(y)-length(coef(mod))
  t = qt(.975,df)
  zz = seq(min(z),max(z),by=.01)
  se.w1 = sqrt(gam1.v + 2*zz*gam1gam3.cv + zz^2*gam3.v)
  w1.hat = coef(mod)["x"]+coef(mod)["x:z"]*zz
  z.tab = cbind(zz,t<abs(w1.hat/se.w1))
  ci.low = w1.hat - t*se.w1
  ci.upp = w1.hat + t*se.w1
  w1.tab = data.frame(w1.hat,z=z.tab[,1],z.tab[,2],ci.low,ci.upp)
  colnames(w1.tab) = c("Est","Z","Significant","LB95", "UB95")
  w1.tab[,3] = ifelse(w1.tab[,3]=="1","Yes","No")
  w1.tab
}
NULL

#' @title Confidence Interval
#' @description Calculates the confidence interval of a vector of data.
#' @keywords univariate
#' @param x a vector of data.
#' @param conf.level confidence level. Default is 0.95.
#' @param alpha confidence level. Default is 1-conf.level.
#' @param \dots Extra parameters.
#' @return
#' \item{CI lower}{Lower bound of interval.}
#' \item{Est. Mean}{Mean of data.}
#' \item{CI upper}{Upper bound of interval.}
#' \item{Std. Error}{Standard Error of the mean.}
#' @examples
#' x = rnorm(1000)
#' ci(x, conf.level=.95)
#'
#' # the piping way
#' mtcars %>% (mpg) %>% ci()
#' @rdname confidenceInterval
#' @export
`ci` <- function(x,conf.level=0.95,alpha=1-conf.level,...)
UseMethod("ci")

#' @export
`ci.default` <- function(x, conf.level=0.95,alpha=1-conf.level,na.rm=FALSE,...)
{
  est <- mean(x, na.rm=na.rm);
  stderr <- sd(x, na.rm=na.rm)/sqrt(nobs(x));
  ci.low <- est + qt(alpha/2, nobs(x)-1) * stderr;
  ci.high <- est - qt(alpha/2, nobs(x)-1) * stderr;
  retval <- c(
    "CI lower"=ci.low,
    "Est. Mean"=est,
    "CI upper"=ci.high,
    "Std. Error"=stderr
  );
  retval;
}

#' @export
`ci.binom` <- function(x, conf.level=0.95,alpha=1-conf.level,...)
{
  if( !(all(x) %in% c(0,1)) ) stop("Binomial values must be either 0 or 1.")
  est <- mean(x, na.rm=TRUE)
  n <- nobs(x)
  stderr <- sqrt(est*(1-est)/n);
  ci.low <- qbinom(p=alpha/2, prob=est, size=n)/n;
  ci.high <- qbinom(p=1-alpha/2, prob=est, size=n)/n;
  retval <- cbind("CI lower"=ci.low,
                  "Est. Mean"=est,
                  "CI upper"=ci.high,
                  "Std. Error"= stderr
  );
  retval;
}

#' @export
`ci.lm` <- function(x,conf.level=0.95,alpha=1-conf.level,...)
{
  x <- summary(x)
  est <- coef(x)[,1] ;
  ci.low <- est + qt(alpha/2, x$df[2]) * coef(x)[,2] ;
  ci.high <- est - qt(alpha/2, x$df[2]) * coef(x)[,2] ;
  retval <- cbind("CI lower"=ci.low,
                  "Est. Mean"=est,
                  "CI upper"=ci.high,
                  "Std. Error"= coef(x)[,2],
                  "p-value" = coef(x)[,4]);
  retval;
}
NULL
