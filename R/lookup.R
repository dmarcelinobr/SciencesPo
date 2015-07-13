#' @encoding UTF-8
#' @title Lookup
#'
#'  @description Recodes values of a vector from a lookup array.
#'
#'  @param x the variable
#'  @param  lookup.array a n-by-2 array used for looking up.
#'
#'
#' @export
`lookup` <- function (x, lookup.array)
{
  if (any(table(lookup.array[, 1]) > 1)) {
    stop("Index value in lookup array not unique!!")
  }
  else{
    b <- rep("", length(x))
    for (i in 1:nrow(lookup.array)) {
      if(is.na(lookup.array[i,1]) & !is.na(lookup.array[i,2])){
        b[is.na(x)] <- lookup.array[i,2]
      }else{
        b[x == lookup.array[i, 1]] <- as.character(lookup.array[i, 2])
      }
    }
    if(is.numeric(lookup.array)){
      x[b != "" & !is.na(b)] <- as.numeric(b[b != "" & !is.na(b)])
    }else{
      x[b != "" & !is.na(b)] <- (b[b != "" & !is.na(b)])
    }
    x[is.na(b)] <- as.numeric(b[is.na(b)])
    answer <- x
    return(answer)
  }
}
NULL
