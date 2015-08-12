#' @title Show Observations Randomly Drawn from the Data
#'
#' @description Function is deprecated, please use \link{view} instead.
#'
#' @param .data The data object.
#' @param ... further parameters.
#'
#' @export
#'
peek <- function(.data = FALSE, ...){
  warning("Function peek() is deprecated. Use view() instead.", call. = FALSE)
  view(.data = .data, ...)
}

weighted.var <- function(x, w, na.rm = FALSE){
    warning("Function weighted.var() is deprecated. Use wtd.var() instead.", call. = FALSE)
    weighted.var(x=x, w=w, na.rm = na.rm)
}
