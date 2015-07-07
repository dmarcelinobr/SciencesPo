#' @encoding UTF-8
#' @title Add Error Bars on the Graph
#'
#' @description Draw error bars on the graph.
#' @param x coordinates for the error bars, or simply the name of the graph.
#' @param y coordinates for the center of the error bars, which is the group means.
#' @param ebl length of the error bars, which should be 1 se in each direction.
#' @param ebu the error bars.
#' @param length the length.
#' @param \dots typically additional noninteresting arguments to pass.
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' x <- c(1:5)
#' y <- c(1.1,1.5,2.9,3.8,5.2)
#' sd <- (0.2,0.3,0.2,0.0,0.4)
#' @export
`errorbars` <- function(x, y, ebl, ebu = ebl, length = 0.08, ...){
  arrows(x, y + ebu, x, y - ebl, angle = 90, code = 3, length= length, ...)
}
NULL






