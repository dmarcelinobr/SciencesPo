#' @title Unfactor columns
#'
#' @description Try to unfactorize columns of a data.frame object.
#' @param .data The data.frame containing factor columns.
#' @examples
#' unfactor(iris)
#'
#' @export
unfactor <- function(.data) {
  if( !is.data.frame(.data)  &&  !is.factor(.data)) stop("unfactor requires a data.frame or vector as input")

  if(is.factor(.data)) {
    out <- levels(.data)[.data]
  } else {
    out <- as.data.frame(sapply(names(.data), function(this.col.name) eval(substitute(.data$thing, list(thing=this.col.name)))), stringsAsFactors=F)
    #if(coerce.numeric) as.data.frame(sapply(names(.data), function(this.col.name) eval(substitute(.data$thing, list(thing=this.col.name)))))
  }
  return(out)
}
