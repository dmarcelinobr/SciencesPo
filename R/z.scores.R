#' @encoding UTF-8
#' @title Compute z-scores
#'
#' @description Compute z-scores
#' @param x a numeric vector
#' @param na.rm a logical indicating whether missing values should be removed
#' @export
#' @examples
#' x <- sample(10)
#' z.scores(x)
`z.scores` <- function( x, na.rm=getOption("na.rm", FALSE) ) {
  ( x - mean(x, na.rm=na.rm)) / sd(x, na.rm=na.rm)
}
NULL
