#' @title Clear Memory of All Objects
#'
#' @description This function is a wrapper for the command \code{rm(list=ls())}.
#'
#' @param what The object (as a string) that needs to be removed (or kept)
#' @param keep Should \code{obj} be kept (i.e., everything but \code{obj} removed)? Or dropped?
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}.
#' @export
#' @examples
#' # create objects
#' a=1; b=2; c=3; d=4; e=5
#' # remove d
#' Clear("d", keep=FALSE)
#' ls()
#' # remove all but a and b
#' Clear(c("a", "b"), keep=TRUE)
#' ls()
`Clear` = function(what = NULL, keep = TRUE) {
  if (!is.null(what)) {
    if (keep) {
      dropme = ls(envir = globalenv())[which(!(ls(envir = globalenv()) %in% what))]
    } else {
      dropme = what
    }
    rm(list = dropme, envir = globalenv())
    cat("All objects were deleted, except:", dropme, sep = ",")
  } else {
    rm(list = ls(envir = globalenv()), envir = globalenv())
    cat("All objects were deleted, including hidden package environments.\n")
  }
}### end -- clear function
NULL