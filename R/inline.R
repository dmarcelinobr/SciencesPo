#' Inline data frame
#'
#' Utility function to inline creation of a data frame
#'
#' @param str text representation of the data frame
#' @param header see \code{\link{read.table}}
#' @param colClasses see \code{\link{read.table}}
#' @param \dots see \code{\link{read.table}}
#' @importFrom utils read.table
#' @export
inline <- function(str,header=TRUE,colClasses=NA, ...){
  read.table( text = str, header=header, colClasses=colClasses, ... )
}
