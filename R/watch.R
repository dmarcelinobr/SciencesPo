#' @title Watch For Changes in a Data Object

#' @param x a variable or a data object.
#' @return a message.
#'@examples
#' a <- 1
#' watch("a")
#' a <- 2
#' @export
#'
watch <- function(x) {
  old <- get(x)
  changed <- function(...) {
    new <- get(x)
    if (!identical(old, new)) {
      message(x, " is now ", new)
      old <<- new
    }
    TRUE
  }
  invisible(addTaskCallback(changed))
}




#' @title Find and download data from SCB (Function is depricated)
#'
#' @description Function is depricated, please use \link{find_scb_data} instead.
#'
#' @param history keep the history when the function is running.
#' @param ... further parameters. These are currently ignored.
#'
#' @export
#'
peek <- function(history = FALSE, ...){
  warning("Function findData() is depricated. Use view() instead.", call. = FALSE)

  view(history = history, ...)

}
