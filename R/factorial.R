#' @encoding UTF-8
#' @title Compute n!
#' @param n The number to be factored out.
#' @examples factorial(5); factorial(5565)
#' @export
`factorial` <- function(n){
  y <- 1
  for(i in 1:n){
    y <-y*((1:n)[i])
  }
  print(y)
}
NULL
