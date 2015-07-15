#' @title fill NA by previous cell value (fill forward)
#' @description fillForward will carry values forward from one observation to the next, filling in missing values with the previous value.
#' @param var the column that contains NAs
#' @note This is not intended for imputing missing values; it is regarded as a bad choice for missing-value imputation. The intent is, rather, to fill in \dQuote{holes}, where a value should naturally prevail from one observation to the next.
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @keywords Missings
#' @examples
#' view(ssex)
#'
#' with(ssex, fill.forward(Favor))
#'
#' @export
`fill.forward` <- function(var) {
  navals <- which(is.na(var))
  filledvals <- which(! is.na(var))
  # If there would be no NAs following each other, navals-1 would give the
  # entries we need. In our case, however, we have to find the last column filled for
  # each value of NA. We may do this using the following sapply trick:
  fillup <- sapply(navals, function(x) max(filledvals[filledvals < x]))
  # And finally replace the NAs with our data.
  var[navals] <- var[fillup]
  var
}
NULL
