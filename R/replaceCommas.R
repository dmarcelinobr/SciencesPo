#' @title Replace commas by dots
#' 
#' @description Replace commas by dots in that order.
#'
#' @param x A vector whose elements contain commas or commas and dots.
#'
#' @details This function works for numeric vectors, typically currency variables stored in non-english format.
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#'
#' @keywords Data Manipulation
#'
#' @examples
#' x <- c('500,00', '0,001', '25.000', '10,100.10', 'him, you, and I.')
#'
#' replaceCommas(x)
#'
#' @export
#'
replaceCommas <- function(x){
  round(as.numeric(gsub(",", ".", gsub("\\.", "", x))),2)
}
