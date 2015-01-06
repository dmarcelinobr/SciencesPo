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

