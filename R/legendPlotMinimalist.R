#' @title Add legends to a plot
#' @description  To add special styles of legends.
#'
#' @param \dots Legend parameters.
#' @export
#' @examples
#' par(mar = c(5, 4, 1.4, 0.2))
#' plot(rnorm(50), rnorm(50), col=c("steelblue", "indianred"), pch=20)
#' legendPlotMinimalist("topright", legend=c("Foo", "Bar"), pch=20,
#' col=c("steelblue", "indianred"),
#' horiz=TRUE, bty='n', cex=0.8)

legendPlotMinimalist <- function(...) {
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0),
              mar=c(0, 0, 0, 0), new=TRUE)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
}
NULL
