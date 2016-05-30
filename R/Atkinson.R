#' @title Atkinson Index of Inequality
#'
#' @description Calculates the Atkinson index A. This inequality measure is especially good at determining which end of the distribution is contributing most to the observed inequality.
#'
#' @param x a vector of data values of non-negative elements.
#' @param n a vector of frequencies of the same length as \code{x}.
#' @param base a parameter of the inequality measure (if \code{NULL}, the default parameter (0.5) of the respective measure is used).
#' @param na.rm logical. Should missing values be removed? Defaults is set to \code{FALSE}.
#' @param \dots additional arguements (currently ignored)
#'
#' @references
#' Cowell, F. A. (2000) Measurement of Inequality in Atkinson, A. B. / Bourguignon, F. (Eds): \emph{Handbook of Income Distribution}. Amsterdam.
#'
#' Cowell, F. A. (1995) \emph{Measuring Inequality} Harvester Wheatshef: Prentice Hall.
#'
#' @seealso \code{\link{Herfindahl}}, \code{\link{Rosenbluth}}, \code{\link{Gini}}. For more details see the \dQuote{Indices} vignette.
#'
#' @examples
#' if (interactive()) {
#' # generate a vector (of incomes)
#' x <- c(778, 815, 857, 888, 925, 930, 965, 990, 1012)
#'
#' # compute Atkinson coefficient with base=0.5
#' Atkinson(x, base=0.5)
#'}
#' @export
#' @rdname Atkinson
`Atkinson` <-function(x, n = rep(1, length(x)), base=NULL, na.rm=FALSE, ...) UseMethod("Atkinson")
NULL

#' @export
#' @rdname Atkinson
`Atkinson.default` <- function(x, n = rep(1, length(x)), base = NULL, na.rm = FALSE, ...){
  x <- rep(x, n)    # same handling as Lc and Gini
  if(na.rm) x <- na.omit(x)
  if (any(is.na(x)) || any(x < 0)) return(NA_real_)

  if(is.null(base)) base <- 0.5
  if(base==1)
    idx <- 1 - (exp(mean(log(x)))/mean(x))
  else
  {
    x <- (x/mean(x))^(1-base)
    idx <- 1 - mean(x)^(1/(1-base))
  }
  print(idx, digits = max(3, getOption("digits") - 3))
}### end -- Atkinson function
NULL
