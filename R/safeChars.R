#' Convert All Factor Columns to Character Columns
#'
#' Sometimes, we forget to use the \code{stringsAsFactors} argument when using
#' \code{\link{read.table}} and related functions. By default, R converts
#' character columns to factors. Instead of re-reading the data, the
#' \code{\link{safeChars}} function will identify which columns are currently
#' factors, and convert them all to characters.
#'
#'
#' @param mydf The name of your \code{data.frame}
#' @author Ananda Mahto
#' @seealso \code{\link{read.table}}
#'
safeChars <- function(mydf) {
  mydf[sapply(mydf, is.factor)] <-
    lapply(mydf[sapply(mydf, is.factor)], as.character)
  mydf
}
