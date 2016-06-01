#' @title Atkinson Index of Inequality
#'
#' @description Calculates the Atkinson index A. This inequality measure is especially good at determining which end of the distribution is contributing most to the observed inequality.
#'
#' @param x a vector of data values of non-negative elements.
#' @param n a vector of frequencies of the same length as \code{x}.
#' @param epsilon a parameter of the inequality measure (if \code{NULL}, the default parameter (0.5) of the respective measure is used).
#' @param na.rm logical. Should missing values be removed? Defaults is set to \code{FALSE}.
#' @param \dots additional arguements (currently ignored)
#' @details
#' epsilon = 0,5: little inequality aversion
#' epsilon = 1,0: medium inequality aversion
#' epsilon = 2,0: great inequality aversion
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
#' # y <- c(80, 60, 10, 20, 30)
#' # Entropy 1.392321
#' # Maximum Entropy	1.609438
#' # Normalized Entropy	0.865098
#' # Exponential Index	0.248498
#' # Herfindahl	0.285000
#' # Normalized Herfindahl	0.106250
#' # Gini Coefficient	0.360000
#' # Concentration Coefficient	0.450000
#'
#' x <- c(778, 815, 857, 888, 925, 930, 965, 990, 1012)
#'
#' # compute Atkinson coefficient with epsilon=0.5
#' Atkinson(x, epsilon=0.5)
#'
#' w <- c(10, 15, 20, 25, 40, 20, 30, 35, 45, 90)
#'}
#'
#' @export
#' @rdname Atkinson
`Atkinson` <-function(x, n = rep(1, length(x)), epsilon=NULL, na.rm=FALSE, ...) UseMethod("Atkinson")
NULL

#' @export
#' @rdname Atkinson
`Atkinson.default` <- function(x, n = rep(1, length(x)), epsilon = NULL, na.rm = FALSE, ...){
  x <- rep(x, n)    # same handling as Lc and Gini
  if(na.rm) x <- na.omit(x)
  if (any(is.na(x)) || any(x < 0)) return(NA_real_)

  if(is.null(epsilon)) epsilon <- 0.5
  if(epsilon==1)
    idx <- 1 - (exp(mean(log(x)))/mean(x))
  else
  {
    x <- (x/mean(x))^(1-epsilon)
    idx <- 1 - mean(x)^(1/(1-epsilon))
  }
  print(idx, digits = max(3, getOption("digits") - 3))
}### end -- Atkinson function
NULL
