#' Inline data.frame
#'
#' @description Utility function to create of a \code{data.frame} inline.
#'
#' @param str text representation of the data frame
#' @param header see \code{\link{read.table}}
#' @param colClasses see \code{\link{read.table}}
#' @param \dots see \code{\link{read.table}}
#' @examples
#' mydf <- inline(str = 'A, B, C, D
#'                       1, 2, 3, 4
#'                      .5, .2, .4, .5', sep=",")
#' @export
inline <- function(str,header=TRUE,colClasses=NA, ...){
  utils::read.table( text = str, header=header, colClasses=colClasses, ... )
}
