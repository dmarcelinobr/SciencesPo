#' @encoding UTF-8
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
#' ageGroups(age.var = x, breaks = c(0, 14, 64, Inf), labels = NULL )
#' ageGroups(age.var = x, breaks = c(0, 14, 64, Inf), 
#' labels = c("<14", "15-64", "65+") )
#' 
#' 
#' ibge_brks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, Inf)
#' ibge_lbls = c("0-4", "5-9", "10-14", "15-19", "20-24",
#'		"25-29", "30-34", "35-39", "40-44", "45-49", "50-54", 
#'  	"55-59", "60-64", "65-69", "70+")
#' ageGroups(age.var = x, breaks = ibge_brks, labels = ibge_lbls )
#'
#'
#' @export
#'
ageGroups <- function (from, to, breaks, labels,
		age.var = NULL) {
    if (is.null(age.var)) {
        age = elapsed(from, to)
    }
    else {
        age = age.var
    }
    ans <- cut(age, breaks = breaks, labels = labels)
    class(ans) <- c("SciencesPo", "factor")
    return(ans) 
}
NULL





#' @encoding UTF-8
#' @title Changes NAs in a vector into a given value
#'
#' @description Changes NAs in a vector into a given value
#' @param x the vector
#' @param value the value to be given to the missing value \code{NA}.
#' @seealso \link{rand.imput}.
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @examples
#' v <- sample(c(round(runif(5, 1, 3)), rep(NA, 2)))
#' NAto0(v)
#'
#'@export
NAto0 <- function(x, value = 0){
  x[is.na(x) == TRUE] <- value
  return(x)
}
NULL




#' @encoding UTF-8
#' @title Eliminate NA observations
#'
#' @description Generates two matrices: One with complete observations and the other with all observations.
#' @param data the \code{data.frame}  
#' 
#' @return two matrices.
#'
#' @seealso \code{\link{complete.cases}}
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @examples
#' id <- 1:10; var1 <- rnorm(10); var2 <- rgamma(10, 2, 1);
#' df <- data.frame(cbind(id, var1, var2))
#' df[c(5, 9), 3] <- NA
#' eliminateNA(df)
#' 
#'@export
eliminateNA <- function(data){
  rows <- dim(data)[1]
  cols <- dim(data)[2]
  tmp <- matrix(NA, ncol = cols, nrow = rows)
  for (i in 1:cols){tmp[, i] <- as.numeric(data[, i])}
  compl <- data[complete.cases(data) == TRUE, ]
  incompl <- data[complete.cases(data) == FALSE, ]
  ans <- list(complete = compl, incomplete = incompl)
  return(ans)
}
NULL




#' @encoding UTF-8
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
NULL


