#' @encoding UTF-8
#' @title Move column order
#' @description Move column order of a data.frame.
#' @param data The data object
#' @param column The variable to be moved.
#' @param where The new location for the variable (column).
#' @examples
#' col.order(ssex, "DK", "Oppose")
#' @export
col.order <- function(data, column, where) {
  m <- data[column]
  r <- data[names(data)!=column]
  i <- match(where,names(data))
  pre <- r[1:i-1]
  post <- r[i:length(names(r))]
  cbind(pre,m,post)
}
NULL
