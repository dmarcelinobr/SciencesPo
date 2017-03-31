#' @title Export a plot as PDF, EPS and PNG at once
#' @description Export a plot as PDF, EPS and PNG at once.
#' @param gplot the graphic object.
#' @param filename the file name.
#' @param width, height dimension parameters.
#' @param dpi the resolution in dpi used when falling back the output.
#' @export
`exportPlot` <- function(gplot, filename, width=2, height=1.5, dpi = 600) {
  # Export plot in PDF and EPS.
  # Notice that A4: width=11.69, height=8.27
  ggsave(paste(filename, '.pdf', sep=""), gplot, width = width, height = height)
  cairo_ps(filename = paste(filename, '.eps', sep=""), width = width, height = height, fallback_resolution = dpi)
  # postscript(file = paste(filename, '.eps', sep=""), width = width, height = height)
  print(gplot)
  dev.off()
  png(file = paste(filename, '_.png', sep=""), width = width * 100, height = height * 100)
  print(gplot)
  dev.off()
}
NULL




#' @title Export data in a tabular file
#' @description Export data in a tabular file.
#' @param data the data object.
#' @param filename the file name.
#' @export
`exportData` <- function(data, filename) {
  # Export data in a tabular file.
  sink(paste(filename, '.txt', sep=""), append=FALSE, split=FALSE)
  print(data)
  sink()
}
