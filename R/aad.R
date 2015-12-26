#' @encoding UTF-8
#' @title Average (Mean) Absolute Deviation
#' @description Calculates the average (mean) absolute deviation from the sample mean.
#' @param x	A numeric vector containing the observations.
#' @param na.rm A logical value for \code{na.rm}, default is \code{na.rm=TRUE}.
#' @details The statistical literature has not yet adopted a standard notation, as both the "Mean absolute deviation around the mean" and the "Median absolute deviation around the median" have been denoted as "MAD", which may lead to confusion once they may produce different values. Thus, the \code{aad} version calculates the "Mean Absolute Deviation"--or  "Average Deviation (AD)" ad suggested by Garrett, who defines it as "the mean of the deviation of all the separate scores in the series taken from their mean (occasionally from the median or mode)", (1971, p. 481).
#' The R \code{\link[stats]{mad}} by default computes the "Median Absolute Deviation" with an adjusting factor for asymptotically normal consistency. To obtain the "Mean Absolute Deviation" one should use \code{stats::mad(x, constant = 1)}.
#'
#' @references
#' Garrett, Henry (1982) \emph{Statistics in Psychology and Education}. 6th, Paragon.
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @keywords Exploratory
#' @seealso \code{\link[stats]{mad}}
#' @examples
#' x <- c(15, 10, 6, 8, 11)
#' aad(x)
#'
#' @export aad
#' @docType methods
#' @rdname aad-methods
#' @aliases aad,numeric,logical,aad-method
`aad`<- setClass("aad", representation(x = "numeric",na.rm="logical"))
setGeneric("aad", def=function(x, na.rm = TRUE){
  standardGeneric("aad")
})

#' @rdname aad-methods
setMethod(f="aad", definition=function(x, na.rm = TRUE){
  if (!is(x, "numeric") & !is(x, "integer")) {
    stop("\"x\" must be numeric")
  }
  if (!is(na.rm, "logical") | length(na.rm) != 1) {
    stop("\"na.rm\" must be a single logical value")
  }
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  ans <- mean(abs(x - mean(x)))
  return(ans)
})## -- end of aad
NULL
