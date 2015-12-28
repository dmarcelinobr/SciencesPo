#' @encoding UTF-8
#' @title Calculates the Standard Error
#'
#' @description Compute the standard errors of a numeric vector
#'
#' @param x  A vector of class numeric or integer
#' @param na.rm A logical value for \code{na.rm}, default is \code{na.rm=TRUE}.
#' @details The standard error of the mean (SEM) (\emph{assuming statistical independence of the values in the sample}) is estimated by taking the standard deviation of the population sample, divided by the square root of the sample size: \deqn{se = \frac{{s}}{{\sqrt{n}}}}
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @examples
#' x <- c(1, 2.3, 2, 3, 4, 8, 12, 43, -1,-4)
#' myse <- sd(x)/sqrt(length(x))
#' myse
#' # With the 'se' function:
#' se(x)
#' @export
#' @docType methods
#' @rdname se-methods
se<- setClass("se", representation(x = "numeric",na.rm="logical"))
setGeneric("se", def=function(x, na.rm = TRUE){
  standardGeneric("se")
})

#' @rdname se-methods
setMethod(f="se", definition=function(x, na.rm = TRUE){
  if (!is(x, "numeric") & !is(x, "integer")) {
    stop("\"x\" must be numeric")
  }
  if (!is(na.rm, "logical") | length(na.rm) != 1) {
    stop("\"na.rm\" must be a single logical value")
  }
  valid <- function(x) return(sum(!is.na(x)))
  valid_ <-function(x) return(ifelse(na.rm,sum(!is.na(x)),length(x)))
  dim <- dim(x)
  if (is.null(dim)) {
    sd <- stats::sd(x, na.rm = na.rm)
    n.valid <- valid_(x)
  }
  else {
    if (is.data.frame(x)) {
      n.valid <- unlist(sapply(x, valid_))
      sd <- unlist(sapply(x, stats::sd, na.rm = na.rm))
    }
    else {
      n.valid <- unlist(apply(x, 2, valid))
      sd <- unlist(apply(x, 2,  stats::sd, na.rm = na.rm))
    }
  }
  return(sd/sqrt(n.valid))
})
NULL
