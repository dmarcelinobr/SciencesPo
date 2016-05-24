#' @title Compute Weighted Correlations
#' @description Compute the weighted correlation.
#' @useDynLib SciencesPo
#' @export
#' @param x a matrix or vector to correlate with \code{y}.
#' @param y a matrix or vector to correlate with \code{x}. If \code{y} is NULL, \code{x} will be used instead.
#' @param weights an optional vector of weights to be used to determining the weighted mean and variance for calculation of the correlations.
#'
#' @examples
#'  x <- sample(10,10)
#'  y <- sample(10,10)
#'  w <- sample(5,10, replace=TRUE)
#'
#' WeightedCorrelation(x, y, w)
#'
`WeightedCorrelation` <- function(x, y=NULL, weights=NULL){
  if(is.null(y)){
    y <- x
  }
  q <- as.matrix(x)
  r <- as.matrix(y)
  if(is.null(weights)){
    weights <- rep(1, dim(q)[1])
  }
  x <- q[!is.na(weights),]
  y <- r[!is.na(weights),]
  weights <- weights[!is.na(weights)]
  out <- .Call("wcorr", as.matrix(x), as.matrix(y), as.double(weights), NAOK=TRUE, PACKAGE="SciencesPo")
  ## C code for this package was contributed by Marcus Schwemmle
  if(!is.null(colnames(x)))
    rownames(out) <- colnames(x)
  if(!is.null(colnames(y)))
    colnames(out) <- colnames(y)
  out
}
NULL



#' @encoding UTF-8
#' @title Convert All Factor Columns to Character Columns of a Data Frame
#'
#' @description By default, R converts character columns to factors.
#' Instead of re-reading the data using \code{stringsAsFactors}, the
#' \code{\link{Safechars}} function will identify which columns are currently factors, and convert them all to characters.
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}.
#'
#' @param .data a \code{data.frame}.
#' @seealso \code{\link{read.table}}, \code{\link{Destring}}.
#' @keywords internal
#' @examples
#'  str(iris)
#' iris_2 = Safechars(iris)
#' str(iris_2)
#'
#' @export
`Safechars` <- function(.data) {
  .data[sapply(.data, is.factor)] <-
    lapply(.data[sapply(.data, is.factor)], as.character)
  .data
}### end -- Safechars function
NULL




#' @encoding UTF-8
#' @title Some Formats for Nicer Display
#' @description Some predefined formats for nicer display.
#' @param x a numeric vector.
#' @param style a character name for style. One of "USD", "BRL", "EUR", "Perc".
#' @param digits an integer for the number of significant digits to be used for
#' numeric and complex x
#' @param flag a character string giving a format modifier as "-", "+", "#".
#' @param nsmall an integer for the minimum number of digits to the right of
#' the decimal point.
#' @param decimal.mark decimal mark style to be used with Percents (\%), usually (",") or (".").
#'
#' @examples
#' x <- as.double(c(0.1, 1, 10, 100, 1000, 10000))
#' Formatted(x)
#'
#' Formatted(x, "BRL")
#'
#' Formatted(x, "EUR")
#'
#' p = c(0.25, 25, 50)
#'
#' Formatted(p, "Perc", flag="+")
#'
#' Formatted(p, "Perc", decimal.mark=",")
#'
#' @export
`Formatted` <- function(x, style=c("USD", "BRL", "EUR", "Perc"),
                        digits = 2, nsmall = 2, decimal.mark = getOption("OutDec"), flag=""){

  style <- .Match(arg = style, choices = c("usd", "brl", "eur", "perc") )

  if (style == "usd"){
   out <- paste("\u0024",formatC(x, digits = digits, format = "f"))
  }
  else if (style=="brl") {
  out <- paste("\u0052\u0024",formatC( x, digits = digits, format = "f", big.mark = ".", decimal.mark = ","))
  }
  else if (style=="eur") {
    out <-  paste("\u20ac",formatC(x, digits = digits, format = "f"))
  }
  else if (style == "perc") {
    out <-  paste(formatC(x, digits = digits, decimal.mark =  decimal.mark, format = "f", flag = flag, drop0trailing = TRUE),"\u0025", sep="")
  }
  else {
    warning(paste(style), " is not a valid style name. See `details` in the function documentation.")
  }
  out
}
NULL
