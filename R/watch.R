#' @title Watch For Changes in a Data Object
#'
#'@examples
#' a <- 1
#' watch("a")
#' a <- 2
#' @export
#'
watch <- function(varname) {
  old <- get(varname)

  changed <- function(...) {
    new <- get(varname)
    if (!identical(old, new)) {
      message(varname, " is now ", new)
      old <<- new
    }
    TRUE
  }
  invisible(addTaskCallback(changed))
}

