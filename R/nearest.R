#' @encoding UTF-8
#' @title Find Location of the Nearest Value
#' @description Find the location of the nearest value to a number that you specify.
#' @param x A vector.
#' @param value The value that you want to find.
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @export
`nearestPosition` <- function(x, value){
  which(abs(x - value) == min(abs(x - value)))
}
NULL



#' @encoding UTF-8
#' @title Find the Nearest Value
#'
#' @description Find the the nearest value to a number that you specify.
#' @param x A vector.
#' @param value The value that you want to find.
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @export
`nearest` <- function(x, value){
  nearloc <- nearestPosition(x, value)
  x[nearloc]
}
NULL
