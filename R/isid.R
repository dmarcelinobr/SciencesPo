#' @title Finds ID combination
#' 
#' @description Finds unique id combination
#' 
#' @param columns columns to combine 
#' @param data the data object
#' @param verbose if \code{TRUE} messages maybe be displayed.
#' 
#' @details The original function was published at \url{https://stackoverflow.com/questions/16045161/unique-identifier-in-data-table}. This was berely touched. 
#' 
#' @export
isid <- function(columns, data, verbose  = TRUE){
  if(!is.data.table(data)){
    copyd <- data.table(data)
  } else{ 
    copyd <- copy(data)
  }
  if(haskey(copyd)){
    setkey(copyd, NULL)
  }
  # NA values don't work in keys for data.tables
  any.NA <- Filter(columns, f= function(x) any(is.na(copyd[[x]])))
  if(verbose){
    for(aa in seq_along(any.NA)){message(sprintf('Column %s contains NA values', any.NA[aa] ))}
  }
  validCols <- setdiff(columns, any.NA)
  # cycle through columns 1 at a time
  ncol <- 1L
  validKey <- FALSE
  while(!isTRUE(validKey) && ncol <= length(validCols)){
    anyValid <- combn(x = validCols, m = ncol, FUN = function(xn){
      subd <- copyd[, xn, with = FALSE]
      result <- nrow(subd) == nrow(unique(subd))
      list(cols = xn, valid = result)
    }, simplify = FALSE)
    
    whichValid <- sapply(anyValid, `[[`, 'valid')
    validKey <- any(whichValid)
    ncol <- ncol + 1L
  }
  
  if(!validKey){
    warning('No combinations are unique')
    return(NULL)} else {
      valid.combinations <- lapply(anyValid, `[[`, 'cols')[whichValid]
      if(length(valid.combinations) > 1){
        warning('More than one combination valid, returning the first only')
      }
      return(valid.combinations[[1]])
    }
}