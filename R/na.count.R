#' @title Count NA values
#' @description The function counts NAs for each column of a \code{data.frame}.
#' @param .data The data.frame.
#'
#' @examples
#'  na.count(ssex)
#'
#' @export
na.count <- function(.data) {
  for (j in colnames(.data)) {
    NAcount <- 0
    NAcount <- as.numeric(sum(is.na(.data[,j])))
    if(NAcount > 0) {
      cat("\n")
      cat(j, ":", NAcount, "missing values")
    } else {
      cat("\n")
      cat(j, ":", NAcount, "missing values")
    }
  }
}
NULL
