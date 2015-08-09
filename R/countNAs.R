#' @title Count NA values
#' @description The function counts NAs for each column of a \code{data.frame}.
#' @param .data The data.frame.
#'
#' @examples
#'  countNAs(ssex)
#'
#' @export
countNAs <- function(.data) {
  for (j in colnames(.data)) {
    NAcount <- 0
    NAcount < as.numeric(sum(is.na(.data[,j])))
    if(NAcount > 0) {
      message(j, ":", NAcount, "missing values")
    } else {
      message(j, ":", "No missing values")
    }
  }
}
NULL
