#' @encoding UTF-8
#' @title Method for Bootstrapping
#' @description this method provides bootstrapping statistics.
#' @param x is a vector or a fitted model object whose parameters will be used to produce bootstrapped statistics. Model objects are assumed to be of class \dQuote{glm} or \dQuote{lm}.
#' @param nboots an integer for the number of reiteration.
#' @param FUN a statistic function name to bootstrap, i.e., 'mean', 'var', 'cov', etc.
#' @param \dots further arguments passed to or used by other methods.
#' @return A list with \dQuote{alpha} and \dQuote{beta} slots. The \dQuote{alpha}
#'  corresponds to ancillary parameters and \dQuote{beta} to systematic components
#'  of the model.
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @keywords Modelling
#' @examples
#' x = runif(10, 0, 1)
#' Bootstrap(x, FUN=mean)
#'
#' @rdname Bootstrap
#' @export
`Bootstrap` <- function (x, ...) UseMethod("Bootstrap")

#' @rdname Bootstrap
#' @export
`Bootstrap.default` <- function(x, nboots = 30, FUN,  ...) {
  n = length(x)
  lings <-
    replicate(nboots, match.fun(FUN)(sample(x, n, replace = TRUE)))
  list(se = SD(lings), mu = Mean(lings), lings = lings)
}
NULL


#' @rdname Bootstrap
#' @export
`Bootstrap.model` <- function (x, ...)
  list(alpha = NULL,
       beta = coef(x))

