#' @title Replace Commas by Dots
#' 
#' @param var a vector whose elements contain commas or commas and dots.
#' @detail This works for numeric vectors, typically currency variables stored in non-english format.
#' @export
#' @examples
#' x <- c('500,00', '0,001', '25.000', '10,100.10', 'him, you, and I.')
#' replace.commas(x)

replace.commas <- function(var){
  round(as.numeric(gsub(",", ".", gsub("\\.", "", var))),2)
}
