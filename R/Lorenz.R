#' @encoding UTF-8
#' @title The Lorenz Curve
#'
#' @description Computes the (empirical) ordinary and generalized Lorenz curve of a vector.
#'
#' @param x A vector of non-negative values.
#' @param n A vector of frequencies of the same length as \code{x}.
#' @param plot A logical. If TRUE the Lorenz curve will be plotted.
#' @param \dots Additional arguements (currently ignored)
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}.
#'
#' @details The Gini coefficient ranges from a minimum value of zero, when all individuals are equal, to a theoretical maximum of one in an infinite population in which every individual except one has a size of zero. It has been shown that the sample Gini coefficients originally defined need to be multiplied by n/(n-1) in order to become unbiased estimators for the population coefficients.
#'
#' @keywords Diversity, Concentration, Viz
#'
#' @seealso \code{\link{Gini}}, \code{\link{GiniSimpson}}.
#' @examples
#' # generate a vector (of incomes)
#' x <- c(778, 815, 857, 888, 925, 930, 965, 990, 1012)
#' # compute Lorenz values
#' Lorenz(x)
#' # generate some weights:
#' wgt <- runif(n=length(x))
#' # compute the lorenz with especific weights
#' Lorenz(x, wgt)
#'
#' @rdname Lorenz
#' @export
`Lorenz` <- function(x, n = rep(1, length(x)), plot = FALSE, ...)
  UseMethod("Lorenz")

#' @export
#' @rdname Lorenz
`Lorenz.default` <- function(x, n = rep(1, length(x)), plot = FALSE, ...)
{
  ina <- !is.na(x)
  n <- n[ina]
  x <- as.numeric(x)[ina]
  k <- base::length(x)
  o <- base::order(x)
  x <- x[o]
  n <- n[o]
  x <- n * x
  p <- base::cumsum(n)/sum(n)
  L <- base::cumsum(x)/sum(x)
  p <- c(0, p)
  L <- c(0, L)
  L2 <- L * base::mean(x)/mean(n)
  idx <- list(p, L, L2)
  names(idx) <- c("p", "L", "L.general")
  class(idx) <- ("lorenz")
 invisible(return(idx))
  if (plot)
    graphics::plot(idx)
 # print(idx, digits = max(3, getOption("digits") - 3))
}##-end of Lorenz
NULL
