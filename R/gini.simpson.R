#' @encoding UTF-8
#' @title Gini Simpson Index
#'
#' @description Computes the Gini/Simpson coefficient.
#'
#' @param x A data.frame, a matrix-like, or a vector.
#' @param na.rm A logical value to deal with NAs.
#'
#' @details The Gini-Simpson quadratic index is a classic measure of diversity, widely used by social scientists and ecologists. The Gini-Simpson is also known as Gibbs-Martin index in sociology, psychology and management studies, which in turn is also known as the Blau index. The Gini-Simpson index is computed as \eqn{1 - \lambda = 1 - \sum_{i=1}^R p_i^2 = 1 - 1/{}^2D}.
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}.
#' @keywords Diversity, Concentration, Inequality
#' @importFrom stats na.omit
#' @seealso \code{\link{politicalDiversity}}.
#' @examples
#' if (interactive()) {
#' # generate a vector (of incomes)
#' x <- as.table(c(69,50,40,22))
#' rownames(x) <- c("AB","C","D","E")
#' gini.simpson(x)
#' }
#' @export gini.simpson
#' @docType methods
#' @rdname gini.simpson-methods
#' @aliases gini.simpson,numeric,logical,ANY-method
`gini.simpson`<-setClass("gini.simpson", representation(x = "numeric", na.rm="logical"))
setGeneric("gini.simpson", def=function(x, na.rm = TRUE){
  standardGeneric("gini.simpson")
})

#' @rdname gini.simpson-methods
setMethod(f="gini.simpson", definition=function(x, na.rm = TRUE){
  # reference: Sachs, Angewandte Statistik, S. 57
  if(na.rm) x <- na.omit(x)
  x <- as.table(x)
  ptab <- prop.table(x)
  return(sum(ptab*(1-ptab)))
})
NULL




#' @encoding UTF-8
#' @title Weighted Gini Index
#'
#' @description Computes the unweighted and weighted Gini coefficient.
#'
#' @param x A data.frame, a matrix-like, or a vector.
#' @param weights A vector containing weights for \code{x}.
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}.
#' @keywords Diversity, Concentration
#' @seealso \code{\link{gini.simpson}}.
#' @examples
#' if (interactive()) {
#' # generate a vector (of incomes)
#' x <- c(778, 815, 857, 888, 925, 930, 965, 990, 1012)
#' # compute Gini index
#' gini(x)
#' # generate some weights:
#' wgt <- runif(n=length(x))
#' # compute the weighted Gini index
#' gini(x, wgt)
#' }
#'
#' @export gini
#' @docType methods
#' @rdname gini-methods
#' @aliases gini,numeric,numeric,ANY-method
`gini`<-setClass("gini", representation(x = "numeric", weights="numeric"))
setGeneric("gini", def=function(x, weights = rep(1, length = length(x))){
  standardGeneric("gini")
})

#' @rdname gini-methods
setMethod(f="gini", definition=function(x, weights = rep(1, length = length(x))){
  ox <- order(x)
  x <- x[ox]
  weights <- weights[ox]/sum(weights)
  p <- cumsum(weights)
  nu <- cumsum(weights * x)
  n <- length(nu)
  nu <- nu/nu[n]
  sum(nu[-1] * p[-n]) - sum(nu[-n] * p[-1])
})
NULL

# http://stats.stackexchange.com/questions/68940/how-is-the-weighted-gini-criterion-defined



# Original Zeileis:
# Gini <- function(x)
# {
#   n <- length(x)
#   x <- sort(x)
#   G <- sum(x * 1:n)
#   G <- 2*G/(n*sum(x))
#   G - 1 - (1/n)
# }

# other:
# http://rss.acs.unt.edu/Rdoc/library/reldist/html/gini.html
# http://finzi.psych.upenn.edu/R/library/dplR/html/gini.coef.html


# Gini <- function(x, n = rep(1, length(x)), unbiased = TRUE, conf.level = NA, R = 1000, type = "bca", na.rm = FALSE) {

#  x <- rep(x, n)    # same handling as Lc
#  if(na.rm) x <- na.omit(x)
#  if (any(is.na(x)) || any(x < 0)) return(NA_real_)
#
#  i.gini <- function (x, unbiased = TRUE){
#    n <- length(x)
#    x <- sort(x)
#
#    res <- 2 * sum(x * 1:n) / (n*sum(x)) - 1 - (1/n)
#    if(unbiased) res <- n / (n - 1) * res
#
#    # limit Gini to 0 here, if negative values appear, which is the case with
#    # Gini( c(10,10,10))
#    return( pmax(0, res))
#
#    # other guy out there:
#    #     N <- if (unbiased) n * (n - 1) else n * n
#    #     dsum <- drop(crossprod(2 * 1:n - n - 1, x))
#    #     dsum / (mean(x) * N)
#    # is this slower, than above implementation??
#  }
#
#  if(is.na(conf.level)){
#    res <- i.gini(x, unbiased = unbiased)
#
#  } else {
#    # adjusted bootstrap percentile (BCa) interval
#    boot.gini <- boot(x, function(x, d) i.gini(x[d], unbiased = unbiased), R=R)
#    ci <- boot.ci(boot.gini, conf=conf.level, type=type)
#    res <- c(gini=boot.gini$t0, lwr.ci=ci[[4]][4], upr.ci=ci[[4]][5])
#  }
#
#  return(res)
#
#}


