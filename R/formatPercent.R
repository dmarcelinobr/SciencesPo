#' @title Format a numeric proportion
#'
#' @description Takes a number and formats it as a percentage.
#' @param x a number or a vector whose numbers will be formated. 
#' @param digits the number of digits to be left.
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @examples
#' x <- c(.15, .00556, .55, 0.246)
#' formatPercent(x, 0)
#' 
#' @export
formatPercent <- function(x, digits = 1){
  ans <- paste(formatC(x * 100, digits, format = "f"), "%", sep = "")
  return(print.noquote(ans))
}
