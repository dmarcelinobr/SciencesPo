
#' Method for recoding
#'
#' This utility function will recode values from an original \code{\link{character}}
#' or \code{\link{factor}} vector with new values.
#'
#' @param x The vector whose values will be recoded.
#' @param \dots The parameters to be used in recode.
#'
#' @export
`recode` <- function(x,...){
  UseMethod("recode")
}
NULL
