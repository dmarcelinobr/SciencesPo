#' @encoding UTF-8
#'   @title Move variable order in a data.frame
#' @param data The data object
#' @param var The variable to be moved.
#' @param where The new location for the variable (var).
#' @examples
#' var.order(ssex, "DK", "Oppose")
#' @export
var.order <- function(data,var,where) {
  m <- data[var]
  r <- data[names(data)!=var]
  i <- match(where,names(data))
  pre <- r[1:i-1]
  post <- r[i:length(names(r))]
  cbind(pre,m,post)
}
NULL
