#' @encoding UTF-8
#' @title To Make Anonymous a Data Frame
#'
#' @description Replaces factor and character variables by a
#' combination of random sampled letters and numbers.
#' Numeric columns are also transformed, see details.
#' @param .data A vector or a data frame.
#' @param col.names A string for column names.
#' @param row.names A string for rown names, default is empty.
#' @return An object of the same type as \code{.data}.
#'
#' @details Strings will is replaced by an algorithm with 52/102 chance of
#' choosing a letter and 50/102 chance of choosing a number; then it joins
#' everything in a 5-digits long character string.
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}.
#'
#' @examples
#' dt <- data.frame(
#' Z = sample(LETTERS,10),
#' X = sample(1:10),
#' Y = sample(c("yes", "no"), 10, replace = TRUE)
#' )
#' dt;
#'
#' Anonymize(dt)
#'
#' @export
#' @rdname Anonymize
`Anonymize` <- function(.data, col.names = "V", row.names = "")
  UseMethod("Anonymize")

#' @rdname Anonymize
#' @export
`Anonymize.default` <- function(.data, col.names = "V", row.names = "") {
  .tweak <- function(x) {
    if(is.factor(x)) {
      levels(x) <- sample(LETTERS, length(levels(x)), replace=TRUE)
    }
    else if(is.numeric(x)) {
      x <- x/mean(x, na.rm=T)
    } else {
      x <- replicate(length(x),
                     paste(sample(c(rep(0:9,each=5),LETTERS,letters), 5,
                                  replace=TRUE), collapse=""))
    }
    return(x)
  }
  ## replace the variable names
  colnames(.data) <- paste(col.names, seq_len(ncol(.data)), sep = "")
  ## fudge any factor levels
  df <- data.frame(lapply(.data,  .tweak))
  ## replace rownames
  rownames(df) <- paste(row.names, seq_len(nrow(df)), sep = "")
  return(df)
}
NULL
