#' @title Round numbers with no leading zero
#'
#' @param x A numeric vector of values to be rounded.
#' @param  digits An integer for the number of digits to round to.
#' @param add An optional dichotomous indicator for whether additional digits should be added if no numbers appear in pre-set digit level.
#' @param max Maximum number of digits to be shown if \code{add=TRUE}.
#' @export
#' @examples
#' rounded(seq(0, 1, by=.1))
`rounded` <- function(x, digits=2, add=TRUE, max=(digits+3)){
  y <- round(x, digits=digits)
  yk <- format(y, nsmall=digits)
  nzero <- sum(unlist(y)==0)
  if(add==TRUE){
    while(nzero>0){
      zeros <- y==0
      digits <- digits+1
      y[zeros] <- round(x, digits=digits)[zeros]
      yk[zeros] <- format(y[zeros], nsmall=digits)
      nzero <- sum(y==0)
      if(digits>(max-1))
        nzero <- 0
    }
  }
  z <- sub("^([-]?)0[.]","\\1.", gsub(" +", "", yk))
  z
}
