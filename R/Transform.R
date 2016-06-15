#' @title Functions for transformation of variables.
#' @description Functions for transformation of variables.
#' @param .data data.table or data.frame object.
#' @param vars Character or vector of characters with variables names to transform.
#' @return Data.table object with transformed variables.
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}.
#' @importFrom data.table :=
#' @examples
#' Factorize(mtcars, c("cyl", "mpg"))
#' @name Transform
#' @rdname Transform
#' @export
`Factorize` <- function(.data, vars) {
  .data <- data.table::data.table(.data)
  .data[ , vars := lapply(.data[ , vars, with=FALSE], as.factor), with=FALSE][]
}
NULL


#' @rdname Transform
#' @export
`Destring` <- function(.data, vars) {
  .data <- data.table::data.table(.data)
  .data[ , vars := lapply(.data[ , vars, with=FALSE], as.numeric), with=FALSE][]
}



#' @encoding UTF-8
#' @title Convert All Factor Columns to Character Columns of a Data Frame
#'
#' @description By default, R converts character columns to factors.
#' Instead of re-reading the data using \code{stringsAsFactors}, the
#' \code{\link{Safechars}} function will identify which columns are currently factors, and convert them all to characters.

#'
#' @param .data a \code{data.frame}.
#' @seealso \code{\link{read.table}}, \code{\link{Destring}}.
#' @keywords internal
#' @examples
#'  str(iris)
#' iris_2 = Safechars(iris)
#' str(iris_2)
#'
#' @rdname Transform
#' @export
`Safechars` <- function(.data) {
  .data[sapply(.data, is.factor)] <-
    lapply(.data[sapply(.data, is.factor)], as.character)
  .data
}### end -- Safechars function
NULL
