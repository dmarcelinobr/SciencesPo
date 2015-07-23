#' @encoding UTF-8
#' @title Modify data elements by their position
#'
#' @description Modify an element in a vector, taking its position as reference.
#'
#' @param x A data object
#' @param position The position of the element to be replaced
#' @param value The value to modify
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#'
#' @keywords Data Manipulation
#
#' @examples
#'
#' x <- seq(1:10)
#'
#' modify(x, 1, 10)
#'
#' @export
#'
`modify` <-
  function(x, position, value) {
    x[position] <- value
    x
  }
NULL
