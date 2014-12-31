#' @title Make Data Anonymous
#' 
#' @description This function replaces factor and character variables by a combination of letters and numbers, and numeric columns are also transformed.
#' 
#' @param x A vector or a data frame
#' 
#' @param keep.names A logical argument. If \code{FALSE}, variable names will be replaced by Xs
#' 
#' @details By making difficult to recognize the original data while keeping the same data structure, this function is  quite useful for sharing data on help lists.
#'
#' @return An object of the same type as \code{x}
#' 
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#'
#' @examples
#' # setup data
#' data(ssex)
#' anonymize(ssex)
#' anonymize(ssex, keep.names=FALSE)
#'
#' @keywords Tables
#'
#' @export
#'
anonymize <-
function(x, keep.names=TRUE){
  truenames <- names(x)
  if(length(x)>26){
   # letters <-replicate(floor(length(x)/26),{letters <-c(LETTERS, paste(LETTERS, LETTERS, sep=""))})
  }
  names(x)<-paste(sample(letters[1:length(x)]))
  level.x<-function(x){
    level.obs<-function(i){
      if(class(x[,i])=="factor" | class(x[,i])=="character"){
        var <-paste(names(x)[i],as.numeric(as.factor(x[,i])), sep="")
        }else if(is.numeric(x[,i])){
          var <-x[,i]- mean(x[,i], na.rm=T)}else{var<-x[,i]}
      return(var)
    }
    x <- data.frame(sapply(seq_along(x), level.obs))
    
    if(keep.names==TRUE){
      names(x) <- truenames 
    }else{
      names(x) <- names(x)
    }
    return(x)
  }
  x <-level.x(x)
  return(x)
}
