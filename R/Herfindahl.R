#' @encoding UTF-8
#' @title Herfindahl Index of Concentration
#'
#' @description Calculates the Herfindahl Index of concentration.
#'
#' @param x a vector of data values of non-negative elements.
#' @param n a vector of frequencies of the same length as \code{x}.
#' @param base a parameter of the concentration measure (if \code{NULL}, the default parameter (1.0) is used instead).
#' @param na.rm a logical. Should missing values be removed? The Default is set to \code{na.rm=FALSE}.
#' @param \dots additional arguements (currently ignored)
#'
#' @details This index is also known as the \emph{Simpson Index} in ecology, the \emph{Herfindahl-Hirschman Index (HHI)} in economics, and as the \emph{Effective Number of Parties (ENP)} in political science.
#'
#' @references
#' Cowell, F. A. (2000) Measurement of Inequality in Atkinson, A. B. / Bourguignon, F. (Eds): \emph{Handbook of Income Distribution}. Amsterdam.
#'
#' Cowell, F. A. (1995) \emph{Measuring Inequality} Harvester Wheatshef: Prentice Hall.
#'
#' @seealso \code{\link{Atkinson}}, \code{\link{Rosenbluth}},
#' \code{\link{PoliticalDiversity}}, \code{\link{Gini}}. For more details see the Indices vignette: \code{vignette("Indices", package = "SciencesPo")}
#'
#' @examples
#' # generate a vector (of incomes)
#' x <- c(778, 815, 857, 888, 925, 930, 965, 990, 1012)
#'
#' # compute the Herfindahl coefficient with base=1
#' Herfindahl(x, base=1)
#'
#'
#' @export
`Herfindahl` <- function(x, n = rep(1, length(x)), base = 1, na.rm = FALSE, ...) UseMethod("Herfindahl")


#' @export
#' @rdname Herfindahl
`Herfindahl.default` <- function(x, n = rep(1, length(x)), base = NULL, na.rm = FALSE, ...){
  x <- rep(x, n)
  if(na.rm) x <- na.omit(x)
  if (any(is.na(x)) || any(x < 0)) return(NA_real_)

  if(is.null(base))
    m <- 1
  else
  m <- base
  idx <- x/sum(x)
  idx <- idx^(m+1)
  idx <- (sum(idx)^(1/m))
  print(idx, digits = max(3, getOption("digits") - 3))
}### end -- herfindahl function
NULL
