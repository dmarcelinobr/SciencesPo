#' @title Rosenbluth Index of Concentration
#'
#' @description Calculates the Rosenbluth index of concentration, also known as Hall or Tiedemann Indices.
#'
#' @param x A vector of data values of non-negative elements.
#' @param n A vector of frequencies of the same length as \code{x}.
#' @param na.rm A logical. Should missing values be removed? The Default is set to \code{na.rm=FALSE}.
#' @param \dots Additional arguements (currently ignored)
#'
#' @references
#' Cowell, F. A. (2000) Measurement of Inequality in Atkinson, A. B. / Bourguignon, F. (Eds): \emph{Handbook of Income Distribution}. Amsterdam.
#'
#' Cowell, F. A. (1995) \emph{Measuring Inequality} Harvester Wheatshef: Prentice Hall.
#'
#' @seealso \code{\link{Atkinson}}, \code{\link{Herfindahl}}, \code{\link{Gini}}, \code{\link{Lorenz}}. For more details see the Indices vignette: \code{vignette("Indices", package = "SciencesPo")}
#'
#' @examples
#' # generate a vector (of incomes)
#' x <- c(778, 815, 857, 888, 925, 930, 965, 990, 1012)
#'
#' # compute Rosenbluth coefficient
#' Rosenbluth(x)
#'
#' @export
#' @rdname Rosenbluth
`Rosenbluth` <-function(x, n = rep(1, length(x)), na.rm=FALSE, ...)  UseMethod("Rosenbluth")
NULL

#' @export
#' @rdname Rosenbluth
`Rosenbluth.default` <-function(x, n = rep(1, length(x)), na.rm = FALSE, ...){
  x <- rep(x, n)
  if(na.rm) x <- na.omit(x)
  if (any(is.na(x)) || any(x < 0)) return(NA_real_)
  n <- length(x)
  x <- sort(x)
  idx <- (n:1)*x
  idx <- 2*sum(idx/sum(x))
  idx <- (1/(idx-1))
  print(idx, digits = max(3, getOption("digits") - 3))
}### end -- Rosenbluth function
NULL


