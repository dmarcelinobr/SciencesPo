#' Odds Ratio
#'
#' S3 method for odds ratio
#'
#' @param x object from whom odds ratio will be computed
#' @param ... further arguments passed to or from other methods
#' @export

`odds.ratio` <-
  function (x, ...) {
    UseMethod("odds.ratio")
  }
