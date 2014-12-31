#' @title Return a factor variable of age groups
#'
#' @description Return a factor variable using breaks and labels. The function will calculate the
#' age based upon the \code{to} if given, otherwise the \code{age.var} will be used.
#'
#' @param from The date of origin, typically birthdate 
#' @param to The up to date to compute the age
#' @param breaks The numeric break guide for grouping age
#' @param labels The labels for the age groups, can also be set to \code{labels=NULL}
#' @param age.var If an age var is present
#' 
#' @author Daniel Marcelino <dmarcelino@@live.com>
#'
#' @examples
#' # The age groupings used by IBGE (grandes grupos).
#' x <- sample(100)
#' age.groups(age.var = x, breaks = c(0, 14, 64, Inf), labels = NULL )
#' age.groups(age.var = x, breaks = c(0, 14, 64, Inf), 
#' labels = c("<14", "15-64", "65+") )
#' 
#' 
#' brks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, Inf)
#' lbls = c("0-4", "5-9", "10-14", "15-19", "20-24",
#'		"25-29", "30-34", "35-39", "40-44", "45-49", "50-54", 
#'  	"55-59", "60-64", "65-69", "70+")
#' age.groups(age.var = x, breaks = brks, labels = lbls )
#'
#' # Here is another instance
#'set.seed(123)
#' dat <- c(sample(2000000, 500), rnorm(100, 1000000, 1000), 
#'         rnorm(150, 1500000, 100),rnorm(150, 500000, 10),
#'         rnorm(180, 10000, 10), rnorm(10, 1000, 5), 1:10)
#' dens <- density(dat)
#' plot(dens)
#'
#' @export
#'
age.groups <- function (from, to, breaks, labels,
		age.var = NULL) {
    if (is.null(age.var)) {
        age = elapsed(from, to)
    }
    else {
        age = age.var
    }
    cut(age, breaks = breaks, labels = labels)
}
