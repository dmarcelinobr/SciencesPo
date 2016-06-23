#' @title Calibrate Plot
#' @description Plots a calibration plot for overlaping observed versus actual values.
#' @param .data the data frame.
#' @param x,y are the forecast and observed data vectors.
#' @param ci is the size of the confidence interval
#' @param xlab a character name for horizontal axis.
#' @param ylab a character name for vertical axis.
#' @author
#' Daniel Marcelino
#' @export
#' @examples
#' data <- data.frame(x=c(10,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100),
#' y=c(0,05,10,20,25,30,40,50,60,70,75,80,95,100,100,100,100,100))
#'
#' Calibrateplot(data, x, y)
#'
Calibrateplot <- function(.data=NULL, x, y, ci=0.95, xlab="Forecast Probability", ylab="Observed Frequency") {
  df <- data.frame(X = seq(0, 100, 1),
                   Y = seq(0, 100, 1))
  gg <- ggplot()
  gg <- gg + geom_line(data= df, aes(X, Y), size=.75, alpha=.8)
  gg <- gg + geom_abline(intercept=0, slope=1, size=.75)
  gg <- gg + geom_line(data=.data, aes(x, y), size=.75, alpha=.5)
  gg <- gg + geom_point(data=.data, aes(x, y), size=2)
  gg <- gg + scale_x_continuous(breaks = seq(0,100,10))
  gg <- gg + scale_y_continuous(breaks = seq(0,100,10))
  gg <- gg + labs(x=xlab, y=ylab)
 gg
}
NULL

