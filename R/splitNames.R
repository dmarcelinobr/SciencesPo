#' @encoding UTF-8
#' @title Splits name fields
#' @description It does one thing: splits a name allocating the first and last names into two new columns or a list.
#' @param name the name field column.
#' @param data the data.frame name.
#'
#' @return two columns or a list.
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @details The way one may split names is region dependent, so this function may apply to very few contexts. See for instance \url{http://www.w3.org/International/questions/qa-personal-names}
#'
#' @examples
#'  df <- data.frame( name = c("Martin Luther King",
#'  "Nelson Mandela", "Simon Bolivar") )
#'  splitNames(df$name)
#'  df$n<- splitNames(df$name)
#' @export
`splitNames`<- function(name, data=.data){
  .data <- NULL
  #nl <- as.list(1:ncol(data))
  # names(nl) <- names(data)
  # - TODO maybe warn about replacing existing variable with the same names (first and last)
  first = as.character(
    lapply(
      strsplit(
        as.character(
          name), split='\\s+'),
      utils::head, n=1))

  last = as.character(
    lapply(
      strsplit(
        as.character(
          name), split='\\s+'),
      utils::tail, n=1))
  if(!missing(data)){
    return(cbind(data, first, last))
  }else{
    return(cbind(first, last))
  }
}
NULL
