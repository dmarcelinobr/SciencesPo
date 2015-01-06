#' @title Show Randomly Drawn Observations
#' 
#' @description Provide a sly view of the data by randomly draw observations, instead of showing only the first \code{head()} or the last \code{tail()} rows of an object.
#' 
#' @param x A matrix or data.frame object
#' @param n The number of rows to be shown
#'  
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' 
#' 
#' @keywords Tables
#' @examples
#' data(galton)
#' peek(galton)
#' 
#' @export
#' 
peek <- function(x, n = 10) {
  if(is.matrix(x) | is.data.frame(x)) {
    rows <- nrow(x)
    print(x[sort(sample(rows, size = n)),])
  } else {
    cat("'peek' only anticipates matrices and data.frames.\n")
  }
}
