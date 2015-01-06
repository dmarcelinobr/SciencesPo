#' @title Return Elapsed Time in Years
#'
#' @description Return the elapsed time in years.
#'
#' @param from the date of origin, typically birthdate
#' @param to the date up to compute the age
#' @param format the date format see \code{as.Date}
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#'
#' @keywords Data Manipulation
#' @keywords Descriptive Stats
#
#' @examples
#' elapsed(from="1988-12-19", to="2014-12-31", format="%Y-%m-%d")
#'
#' elapsed("1jan1960", "2jan1990", "%d%b%Y")
#'
#' @export
#'
elapsed <- function (from, to, format) {
    round(as.numeric((as.Date(to, format=format) - as.Date(from, format=format))/365.25),1)
}