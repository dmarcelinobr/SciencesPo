#' @encoding UTF-8
#' @title Gini Coefficient G
#'
#' @description Computes the unweighted and weighted Gini index of a distribution.
#'
#' @param x a data.frame, a matrix-like, or a vector.
#' @param weights a vector containing weights for \code{x}.
#' @param \dots additional arguements (currently ignored)
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}.
#'
#' @details One might say the Gini is oversensitive to changes in the middle,
#' while undersensitive at the extremes. The G coefficient doesn't capture very
#' explicitly changes in the top 10% -- which has become the focus of much
#' inequality research in the past 10 years -- or the bottom 40%, where
#' most poverty lies. The alternative Palma ratio does.
#'
#' @keywords Diversity, Concentration
#' @seealso \code{\link{Simpson}}, \code{\link{Lorenz}}, \code{\link{Herfindahl}}, \code{\link{Rosenbluth}}, \code{\link{Atkinson}}..
#' @examples
#' # generate a vector (of incomes)
#' x <- c(778, 815, 857, 888, 925, 930, 965, 990, 1012)
#' # compute Gini coefficient
#' Gini(x)
#'
#' # For Gini index: Gini(x)*100
#'
#' Gini(c(100,0,0,0), c(1,33,33,33))
#'
#' # Considers this
#' A <- c(20000, 30000, 40000, 50000, 60000)
#' B <-  c(9000, 40000, 48000, 48000, 55000)
#'
#'  Gini(A); Gini(B);
#'
#' @export
`Gini` <- function(x, weights, ...) UseMethod("Gini")


#' @export
#' @rdname Gini
`Gini.default` <- function(x, weights = rep(1, length = length(x)), ...){
  # TODO : add unbiased estiamtes see: gini(c(100,0,0,0))
  ox <- order(x)
  x <- x[ox]
  weights <- weights[ox]/sum(weights)
  p <- cumsum(weights)
  nu <- cumsum(weights * x)
  n <- length(nu)
  nu <- nu/nu[n]
  idx <- (sum(nu[-1] * p[-n]) - sum(nu[-n] * p[-1]))
  print(idx, digits = max(3, getOption("digits") - 3))
  # print(paste("Gini Index:", idx*100," Gini Coefficient G:", idx))
}
NULL


# Original Zeileis:
# Gini <- function(x)
# {
#   n <- length(x)
#   x <- sort(x)
#   G <- sum(x * 1:n)
#   G <- 2*G/(n*sum(x))
#   G - 1 - (1/n)
# }


#' @encoding UTF-8
#' @title Gini-Simpson Index
#'
#' @description Computes the Gini/Simpson coefficient. \code{NA}s from the data are omitted.
#'
#' @param x a data.frame, a matrix-like, or a vector.
#' @param na.rm a logical value to deal with NAs.
#' @param \dots additional arguements (currently ignored).
#'
#' @details The Gini-Simpson quadratic index is a classic measure of
#' diversity, widely used by social scientists and ecologists.
#' The Gini-Simpson is also known as Gibbs-Martin index in sociology,
#'  psychology and management studies, which in turn is also known as
#'  the Blau index. The Gini-Simpson index is computed as
#'  \eqn{1 - \lambda = 1 - \sum_{i=1}^R p_i^2 = 1 - 1/{}^2D}.
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}.
#' @keywords Diversity, Concentration, Inequality
#' @importFrom stats na.omit
#' @seealso \code{\link{Gini}}.
#' @examples
#' # generate a vector (of incomes)
#'
#' x <- as.table(c(69,50,40,22))
#'
#' # let's say AB have coalesced
#' rownames(x) <- c("AB","C","D","E")
#'
#' print(x)
#'
#' Simpson(x)
#'
#' @export
`Simpson` <- function(x, na.rm=TRUE, ...) UseMethod("Simpson")

#' @rdname Simpson
#' @export
`Simpson.default` <- function(x,  na.rm = TRUE, ...){
  # reference: Sachs, Angewandte Statistik, S. 57
  if(na.rm) x <- na.omit(x)
  x <- as.table(x)
  ptab <- prop.table(x)
  idx <- sum(ptab*(1-ptab))
  print(idx, digits = max(3, getOption("digits") - 3))
}##--end of gini.simpson
NULL
