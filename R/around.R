#' @title Find the Values Around a Particular Value
#' 
#' @description Find the location of values around a specified value
#' 
#' @param x A vector.
#' @param value Specified value
#' 
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' 
#' @return lo The maximum value of x that is less than or equal to the value parameter.
#' @return hi The minimum value of x that is greater than or equal to the value parameter.
#' 
#' @examples
#' set.seed(123)
#' x = rnorm(25, 5, 10)
#' value = 9
#' around(x, value)
#' 
#' 
#' @export 

around<-function(x, value){
  x<-sort(x)
  lo<-x[nearest.loc(x, value)]
  if(lo>=value)
    lo<-x[nearest.loc(x, value)-1]
  
  hi<-x[nearest.loc(x, value)]
  if(hi<value)
    hi<-x[nearest.loc(x, value)+1]
  
  c(lo, hi)
}
NULL

#' @title Find Location of Nearest Value
#' 
#' @description Find the location of the nearest value to a number that you specify.
#' 
#' @param x A vector.
#' @param value The value that you want to find.
#' 
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @export
nearest.loc<-function(x, value){
  which(abs(x-value)==min(abs(x-value)))
}
NULL



#' @title Find the Nearest Value
#' 
#' @description Find the the nearest value to a number that you specify.
#' 
#' @param x A vector.
#' @param value The value that you want to find.
#' 
#' 
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @export
nearest<-function(x, value){
  nearloc<-nearest.loc(x, value)
  x[nearloc]
}

