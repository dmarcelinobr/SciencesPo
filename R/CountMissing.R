#' @title Count missing data.
#' @description Function to count missing data.
#' @param .data the data frame.
#' @param vars vector with name of variables.
#' @param prop logical. If this is \code{TRUE} will show share of missing data by variable.
#' @param exclude.complete Logical. Exclude complete variables from the output.
#' @examples
#'
#' CountMissing(marriage, prop = FALSE)
#' @export
`CountMissing`  <- function(.data, vars = NULL, prop = TRUE, exclude.complete = TRUE) {

  if (is.null(vars)) {
    vars <- names(.data)
  }
  .data <- data.table::data.table(.data)
  miss <- sort( sapply(.data[, vars, with = FALSE], function(x) sum(is.na(x))), decreasing = TRUE)

  if (exclude.complete == TRUE) {
    miss <- miss[ miss > 0]
  }

  if (prop == FALSE)
  { return(miss) }

  else if ( prop == TRUE )
  { return( round(miss / nrow(.data), 3)) }

  return(miss)

}
NULL
