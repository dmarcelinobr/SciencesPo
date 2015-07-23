#' @encoding UTF-8
#' @title Format numeric digits
#' @param x the object whose values to format
#' @param digits An integer for the number of decimal places.
#' @export
`formatNumbers` <- function (x, digits=2) {
  noZero <- function (x) {
    return(gsub("0\\.", ".", x));
  }
  return(noZero(round(x, digits)));
}
NULL
