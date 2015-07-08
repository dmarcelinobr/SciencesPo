#' @title Converts calendar date string to POSIX
#'
#' @param x character vector in one of two calendar date formats
#' @return a POSIX date
#' @export
#' @author Daniel Marcelino \email{dmarcelino@@live.com}
posixify <- function(x) {
  x <- as.character(x)
  if(any(regexpr("^[[:digit:]]{1,2}/[[:digit:]]{1,2}/[[:digit:]]{4}$", x[1])[1] == 1))
    strptime(x, format="%m/%d/%Y") # short date format
  else
    strptime(x, format="%m/%d/%Y %I:%M:%S %p") # long date-time format
}