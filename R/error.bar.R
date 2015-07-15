#' @encoding UTF-8
#' @title Add Error Bars on the Graph
#'
#' @description Draws error bars.
#' @param x Coordinates for the error bars, or simply the name of the graph.
#' @param y Coordinates for the center of the error bars, which is the group means.
#' @param upper The length of the error bar, typically 1 \code{se} or \code{sd} in each direction.
#' @param lower The error bars.
#' @param length The length of the marks.
#' @param \dots typically additional noninteresting arguments to pass.
#' @keywords Graphics
#'
#' y <- rnorm(1000, mean=1)
#' y <- matrix(y,300,3)
#' y.means <- apply(y,2,mean)
#' y.sd <- apply(y,2,sd)
#'
#' barplot <- barplot(y.means, names.arg=1:3,ylim=c(0,1.5), axis.lty=1)
#' error.bar(barplot,y.means, 1.96*y.sd/10, length = .1)
#'
#' @export
`error.bar` <- function(x, y, upper, lower=upper, length=0.1,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  graphics::arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

NULL






