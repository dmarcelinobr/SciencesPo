#' @title Functions for transformation of variables.
#' @description Functions for transformation of variables.
#' @param .data data.table or data.frame object.
#' @param vars Character or vector of characters with variables names to transform.
#' @return Data.table object with transformed variables.
#' @importFrom data.table :=
#' @examples
#' Factorize(mtcars, c("cyl", "mpg"))
#' @name Transform
#' @rdname Transform
#' @export
`Factorize` <- function(.data, vars) {
  .data <- data.table::data.table(.data)
  .data <- .data[ , vars := lapply(.data[ , vars, with=FALSE], as.factor), with=FALSE]
}
NULL


#' @rdname Transform
#' @export
Destring <- function(.data, vars) {
  .data <- data.table::data.table(.data)
  .data <- .data[ , vars := lapply(.data[ , vars, with=FALSE], as.numeric), with=FALSE]
}
