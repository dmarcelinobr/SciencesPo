#' @title Remove all objects, except those specified
#' @description Stata-like keep function. Remove all objects from the default workspace, except those specified.
#' @param \dots objects to be kept, specified one by one, quoted or unquoted.
#' @param list character vector of object names to be kept.
#' @param sure whether to perform the removal, otherwise return names of objects that would have been removed.
#' @details Implemented with a few safety caps: objects whose name starts with a
#' period \sQuote{\code{.}} are not removed, and \code{sure=TRUE} is
#' required to perform the removal.
#' @export
#' @examples
#' data(women, twins)
#' keep(women)
#' ## To remove all objects except women, run:
#' ## keep(women, sure=TRUE)
#'
`keep` <- function(..., list=character(0), sure=FALSE)
{
  if(missing(...) && missing(list))
    stop("Keep something, or use rm(list=ls()) to clear workspace.")
  names <- as.character(substitute(list(...)))[-1]
  list <- c(list, names)
  keep.elements <- match(list, ls(1))

  if(sure == FALSE)
    return(ls(1)[-keep.elements])
  else
    rm(list=ls(1)[-keep.elements], pos=1)
}
NULL




#' @title Keep all columns, except those specified
#' @description Stata-like drop function. Keep all columns from the data.frame, except those specified.
#'
#' @param .data dataframe to be modified.
#' @param names character vector containing the current name of each variable to be dropped/removed.
#' @param info boolean value indicating whether to print details of the renaming.  Defaults to TRUE.
#' @examples
#' data(women)
#' drop(women, "weight")
#' @export
`drop` <- function( .data, names, info=TRUE)
{
  for( i in names )
  {
    if(info)
      cat("Removing variable '", i, "'\n", sep="")
    .data[[i]] <- NULL
  }
  .data
}
NULL

