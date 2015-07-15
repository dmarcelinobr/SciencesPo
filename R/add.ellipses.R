#' @title Plots ellipses
#' @description Plots ellipses given the coordinates and radio.
#' @param x The longitude coordinate.
#' @param y The latitude coordinate.
#' @param r The radio.
#' @param \dots Extra parameters to plot, such as col, lty etc.
#' @export
#' @examples
#' plot(-60:-30, -30:0)
#'add.ellipses(-49,-25,7)
add.ellipses <- function(x, y, r, ...) {
  angles <- seq(0,2*pi,length.out=360)
  graphics::lines(r*cos(angles)+x,r*sin(angles)+y, ...)
}
NULL
