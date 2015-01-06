#' @title Save Plot to a PNG
#'
#'  @description  Save plots as PNG to working directory
#'  @param file file location
#'  @param width Width for the plot
#'  @param height Height for the plot
#'  @param FUN Function to be plotted
#' @param png a logical argument
#'
as.png <- function(file, width, height, FUN, png=FALSE){
  # output file location, width, height, function to be plotted
  # e.g. pngPlot("C:/..", 6, 5, function(){ ... })
  png(paste(file, ".png", sep=""), width=width*100, height=height*100)
  FUN()
  dev.off()
}