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
  warning("Function findData() is deprecated. Use view() instead.", call. = FALSE)

  view(.data = .data, ...)

}