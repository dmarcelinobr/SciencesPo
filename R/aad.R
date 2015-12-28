#' @encoding UTF-8
#' @title Average Absolute Deviation
#' @description Calculates the average (mean) absolute deviation from the sample mean.
#' @param x	A numeric vector containing the observations.
#' @param na.rm A logical value for \code{na.rm}, default is \code{na.rm=TRUE}.
#' @details The statistical literature has not yet adopted a standard notation, as both the "Mean Absolute Deviation" and the "Median Absolute Deviation" have been denoted as "MAD", which may lead to confusion as they may produce different values.
#' The R \code{\link[stats]{mad}} computes the "Median Absolute Deviation" by default; to obtain the "Mean Absolute Deviation" one has to use \code{mad(x, constant = 1)}.
#' Thus, the function \code{\link[SciencesPo]{aad}} will calculate the "Mean Absolute Deviation"--or "Average Deviation (AD)" as proposed by Garrett, who defines it as "the mean of the deviation of all the separate scores in the series taken from their mean (occasionally from the median or mode)", (1971, p. 481).
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
#' @export
#' @docType methods
#' @rdname aad-methods
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
