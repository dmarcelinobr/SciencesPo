#' @title Fractional part of a numeric vector.
#' @description
#' Split off fractional part of a numeric vector, compute and evaluate continuous fractions.
#' @param x A vector of numerics.
#' @param digits An integer. If not missing, the fractional part will be rounded.
#' @keywords Describe
#' @examples
#'frac(c(0, pi, 2*pi))
#' @export
`frac` <-
  function(x, digits = NA){
   res <- abs(x/sum(x))
    if (!missing(digits))
      res<-round(res, digits=digits)
    res
  }
NULL
