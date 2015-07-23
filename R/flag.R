#' @title Flag duplicated observations
#' @description Marks how many times an observation appears in the dataset.
#' @param obj The data object.
#' @param check.by The formula for checking row-wise for duplicates.
#'
#' @examples
#' df <- data.frame(matrix(c(51,42,43,1,22,51,
#'                  92,28,21,1,22,9),ncol=3, byrow = TRUE))
#' colnames(df) <- c("A","B","C")
#' flag(df, check.by = c("A", "B") )
#' @export
`flag` <- function(obj=.data, check.by=NULL){
  DUPS <- duplicated(obj[, check.by])
  k<-1
  for ( i in 1:nrow(obj)) {
    if(!DUPS[i]) {
      obj$flag[i]<-k
    } else {
      k<-k+1
      obj$flag[i]<-k
    }
  }
  return(obj)
}
NULL
