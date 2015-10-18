#' @encoding UTF-8
#' @title Stukel's test of the logistic link
#' @description The Stukel's test is an alternative to the goodness-of-fit test for logistic regression.
#'  It tests if significant change occurs in the model with the addition of new coefficients.
#'
#' @param object An object of class \code{glm}.
#'
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
#'
#' @importFrom stats predict vcov residuals pchisq
#'
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
#' @title Variance Inflation Factor
#' @description Extracts Variance Inflation Factor from a model of class \dQuote{lm}
#' @param model a model object
#' @param \dots further arguments passed to or used by other methods.
#' @return A numeric value indicating the variance inflation in the model
#' #' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#'
#' @importFrom stats coef vcov terms cov2cor model.matrix coefficients
#'
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
#' @title Geary's test for normality
#' @description This function computes an estimator of Geary's measure of kurtosis.
#' @param x the numeric vector.
#' @param na.rm A logical for NA values.
#' @details Null hypothesis is that the data obeys to normal distribution and that data should have kurtosis equal to 3.
#' @return statistic The Geary's test of statistic G.
#' @return p.value The significant probability of the null-hypothesis testing.
#' @author Daniel Marcelino \email{dmarcelino@@live.com}
#' @examples
#' set.seed(1234)
#' x = rnorm(1000)
#' geary(x)
#'
#' geary(20:50)
#'
#' y = c(0.269, 0.357, 0.2, 0.221, 0.275, 0.277, 0.253, 0.127, 0.246)
#' qqnorm(y)
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




#' @encoding UTF-8
#' @title James-Stein shrunken estimates
#'
#' @description Computes James-Stein shrunken estimates of cell means given
#' a response variable (which may be binary) and a grouping indicator.
#' @references
#' Efron, Bradley and Morris, Carl (1977) ``Stein's Paradox in Statistics.'' \emph{Scientific American} Vol. 236 (5): 119-127.
#'
#' James, Willard and Stein, Charles (1961) ``Estimation with
#' Quadratic Loss.'' \emph{Proceedings of the Fourth Berkeley
#' Symposium on Mathematical Statistics and Probability}, Vol. 1: 361-379.
#'
#' @param y The response variable.
#' @param k The grouping factor.
#' @export
james.stein <- function(y, k)
{
  s <- !(is.na(y)|is.na(k))
  y <- y[s];
  k <- as.character(k[s])
  ## as.char -> unused levels OK
  k <- length(unique(k))
  if(k<3)
    stop("must have >=3 groups")

  .stats <- function(w) {
    bar <- mean(w)
    ss  <- sum((w-bar)^2)
    n <- length(w)
    ##if(n<2)
    ##  stop("a group has n<2")

    c(n=length(w), mean=bar, ss=ss, var=ss/n/(n-1))
  }

  Z <- .stats(y)
  st <- tapply(y, k, FUN=.stats)
  nams <- names(st)
  z <- matrix(unlist(st),ncol=4,byrow=TRUE)
  ssb <- .stats(z[,2])["ss"]
  shrink <- 1 - (k-3)*z[,4]/ssb
  shrink[z[,1]==1] <- 0
  shrink <- pmin(pmax(shrink,0),1)
  list(n=z[,1], mean=z[,2],
       shrunk.mean=structure(Z["mean"]*(1-shrink)+shrink*z[,2], names=nams),
       shrink=shrink)
}
NULL




#' @encoding UTF-8
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
NULL




#' @encoding UTF-8
#' @title Jensen-Shannon Distance
#'
#' @description The Jensen-Shannon divergence or distance matrix stores the \eqn{n*(n-1)/2} pairwise distances/similarities between observations in an \eqn{n x p} matrix where n correspond to the independent observational units and p represent the covariates measured on each individual.
#' @param mat An n x p matrix.
#' @examples
#'# create a matrix
#' n  = 10
#' m = matrix(runif(n*10), ncol = 10)
#' m = m/rowSums(m)
#' jensen.shannon(m)
#' @export
jensen.shannon <- function(mat) {
  kld = function(p,q) sum(ifelse(p == 0 | q == 0, 0, log(p/q)*p))
  res = matrix(0, nrow(mat), nrow(mat))
  for (i in 1:(nrow(mat) - 1)) {
    for (j in (i+1):nrow(mat)) {
      m = (mat[i,] + mat[j,])/2
      d1 = kld(mat[i,], m)
      d2 = kld(mat[j,], m)
      res[j,i] = sqrt(.5*(d1 + d2))
    }
  }
  res
}
NULL




#' @encoding UTF-8
#' @title Johnson-Neyman Regression
#' @description Probing Regression Interactions
#' @param y the dependent variable.
#' @param x the independent variable.
#' @param z the moderator variable.
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @importFrom stats lm vcov coef qt
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





#' @encoding UTF-8
#' @title Lilliefors (Kolmogorov-Smirnov) test for normality
#'
#' @description Performs the Lilliefors (Kolmogorov-Smirnov) test for the composite hypothesis of normality. The Lilliefors (Kolomorov-Smirnov) test is the most famous EDF omnibus test for normality; compared to the Anderson-Darling test and the Cramer-von Mises test it is known to perform worse.
#'
#' @param x A numeric vector of data values, the number of observations must be greater than 4.
#'  Missing values are allowed.
#' @references
#' Thode Jr., H.C. (2002): Testing for Normality. Marcel Dekker, New York.
#'
#' @importFrom stats complete.cases pnorm
#' @export
#' @examples
#' set.seed(1234)
#' x = rnorm(1000)
#' lilliefors(x)
#'
lilliefors <- function(x){
  DNAME <- deparse(substitute(x))
  x <- sort(x[stats::complete.cases(x)])
  n <- length(x)
  if (n < 5)
    stop("sample size must be greater than 4")
  p <- stats::pnorm((x - mean(x))/sd(x))
  Dplus <- max(seq(1:n)/n - p)
  Dminus <- max(p - (seq(1:n) - 1)/n)
  K <- max(Dplus, Dminus)
  if (n <= 100) {
    Kd <- K
    nd <- n
  }
  else {
    Kd <- K * ((n/100)^0.49)
    nd <- 100
  }
  pvalue <- exp(-7.01256 * Kd^2 * (nd + 2.78019) + 2.99587 *
                  Kd * sqrt(nd + 2.78019) - 0.122119 + 0.974598/sqrt(nd) +
                  1.67997/nd)
  if (pvalue > 0.1) {
    KK <- (sqrt(n) - 0.01 + 0.85/sqrt(n)) * K
    if (KK <= 0.302) {
      pvalue <- 1
    }
    else if (KK <= 0.5) {
      pvalue <- 2.76773 - 19.828315 * KK + 80.709644 *
        KK^2 - 138.55152 * KK^3 + 81.218052 * KK^4
    }
    else if (KK <= 0.9) {
      pvalue <- -4.901232 + 40.662806 * KK - 97.490286 *
        KK^2 + 94.029866 * KK^3 - 32.355711 * KK^4
    }
    else if (KK <= 1.31) {
      pvalue <- 6.198765 - 19.558097 * KK + 23.186922 *
        KK^2 - 12.234627 * KK^3 + 2.423045 * KK^4
    }
    else {
      pvalue <- 0
    }
  }
  RVAL <- list(statistic = c(D = K), p.value = pvalue, method = "Lilliefors (Kolmogorov-Smirnov) normality test",
               data.name = DNAME)
  class(RVAL) <- "htest"
  return(RVAL)
}
NULL





#' @encoding UTF-8
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
NULL




#' @encoding UTF-8
#' @title Anscombe-Glynn test of kurtosis
#'
#' @description Performs the Anscombe-Glynn test of kurtosis for normal samples.
#'
#' @param x A numeric vector of data values.
#' @param alternative A character string specifying the alternative hypothesis,
#'  must be one of '"two.sided"' (default), '"greater"' or '"less"'. You can
#'   specify just the initial letter.
#' @details Under the hypothesis of normality, data should have kurtosis equal
#'  to 3.This test has such null hypothesis and is useful to detect a
#'  significant difference of kurtosis in normally distributed data.
#' @references Anscombe, F.J., Glynn, W.J. (1983) Distribution of kurtosis
#' statistic for normal statistics. Biometrika, 70, 1, 227-234
#' @importFrom stats complete.cases  pnorm
#' @export
#' @examples
#' set.seed(1234)
#' x = rnorm(1000)
#' kurtosis(x)
#' anscombe.glynn(x)
`anscombe.glynn` <-
  function (x, alternative=c("two.sided","less","greater"))
  {
    DNAME <- deparse(substitute(x))
    x <- sort(x[stats::complete.cases(x)])
    n <- length(x)
    s <- match.arg(alternative)
    alter <- switch(s, two.sided=0, less=1, greater=2)
    b <- n*sum( (x-mean(x))^4 )/(sum( (x-mean(x))^2 )^2);
    eb2 <- 3*(n-1)/(n+1);
    vb2 <- 24*n*(n-2)*(n-3)/ ((n+1)^2*(n+3)*(n+5));
    m3 <- (6*(n^2-5*n+2)/((n+7)*(n+9)))*sqrt((6*(n+3)*(n+5))/(n*(n-2)*(n-3)));
    a <- 6 + (8/m3) * (2/m3 + sqrt(1 + 4/m3^2));
    xx <- (b-eb2)/sqrt(vb2);
    z <- ( 1-2/(9*a)-( (1-2/a) / (1+xx*sqrt(2/(a-4))) )^(1/3))/ sqrt(2/(9*a));
    pval <- stats::pnorm(z, lower.tail = FALSE)
    if (alter == 0) {
      pval <- 2*pval
      if (pval > 1) pval<-2-pval
      alt <- "kurtosis is not equal to 3"
    }
    else if (alter == 1)
    {
      alt <- "kurtosis is greater than 3"
    }
    else
    {
      pval <- 1-pval
      alt <- "kurtosis is lower than 3"
    }
    RVAL <- list(statistic = c(kurt = b, z = z), p.value = pval,
                 alternative = alt, method = "Anscombe-Glynn kurtosis test",
                 data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
  }
NULL




#' @encoding UTF-8
#' @title Anderson-Darling test for normality
#'
#' @description Performs the Anderson-Darling test for the composite hypothesis of normality. It is the recommended EDF test by Stephens (1986) followed by the Cramer-von Mises test. Compared to the later, the Anderson-Darling gives more weight to the tails of the distribution.
#'
#' @param x A numeric vector of data values, the number of observations must be greater than 7.
#'  Missing values are allowed.
#' @importFrom stats complete.cases  pnorm
#' @export
#' @examples
#' set.seed(1234)
#' x = rnorm(1000)
#' anderson.darling(x)
#'
anderson.darling <- function (x)
{
  DNAME <- deparse(substitute(x))
  x <- sort(x[stats::complete.cases(x)])
  n <- length(x)
  if (n < 8)
    stop("sample size must be greater than 7")
  logp1 <- stats::pnorm((x - mean(x))/sd(x), log.p = TRUE)
  logp2 <- stats::pnorm(-(x - mean(x))/sd(x), log.p = TRUE)
  h <- (2 * seq(1:n) - 1) * (logp1 + rev(logp2))
  A <- -n - mean(h)
  AA <- (1 + 0.75/n + 2.25/n^2) * A
  if (AA < 0.2) {
    pval <- 1 - exp(-13.436 + 101.14 * AA - 223.73 * AA^2)
  }
  else if (AA < 0.34) {
    pval <- 1 - exp(-8.318 + 42.796 * AA - 59.938 * AA^2)
  }
  else if (AA < 0.6) {
    pval <- exp(0.9177 - 4.279 * AA - 1.38 * AA^2)
  }
  else if (AA < 10) {
    pval <- exp(1.2937 - 5.709 * AA + 0.0186 * AA^2)
  }
  else pval <- 3.7e-24
  RVAL <- list(statistic = c(A = A), p.value = pval, method = "Anderson-Darling normality test",
               data.name = DNAME)
  class(RVAL) <- "htest"
  return(RVAL)
}
NULL
