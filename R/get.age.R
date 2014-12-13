#' Returns the age in years based upon the given date.
#' @author Daniel Marcelino <dmarcelino@live.com>
#' @export
#' @examples
#' get.age(from="1988-12-19", to="2014-12-31", format="%Y-%m-%d")
get.age("1jan1960", "2jan1990", "%d%b%Y")
get.age <- function (from, to, format) {
    as.numeric((as.Date(to, format=format) - as.Date(from, format=format))/365.25)
}