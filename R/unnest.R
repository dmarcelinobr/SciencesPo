#' @title Unnest a Nested List
#' 
#' @description  Unnest nested lists made easy.
#' 
#' @param x A nested list
#' 
#' @return A list, with no nesting, hopefully
#'  @author Daniel Marcelino, \email{dmarcelino@@live.com}
#'  
#' @examples
#' # Unnest the list
#' # a nested list
#' mylist <- list(); inerlist <- list()
#' for(i in 1:5) {
#'   for(j in 1:5) {
#'    mylist[[j]] <- i*j
#'  } 
#'  inerlist[[i]] <- mylist
#' }
#' unnest(inerlist)[[1]]
#' unnest(inerlist)
#' 
#' @export 
unnest <-
function(x) {
  if(is.null(names(x))) {
    list(unname(unlist(x)))
  }
  else {
    c(list(all=unname(unlist(x))), do.call(c, lapply(x, unnest)))
  }
}
