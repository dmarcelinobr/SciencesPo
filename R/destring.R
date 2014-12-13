#' Convert Factor Levels into Strings
#' 
#' @param x a factor whose levels will be converted.
#' @export
#' @examples
#' mylevels <- c('Strongly Disagree', 'Disagree', 'Neither', 'Agree', 'Strongly Agree')
#' myvar <- factor(sample(mylevels[1:5], 10, replace=TRUE))
#' unclass(test) # testing order
#' destring(myvar)
destring <- function(x) {
## convert factor to strings
if(is.character(x)) {
as.numeric(x)
} else if (is.factor(x)) {
as.numeric(as.factor(x))
} else if (is.numeric(x)) {
invisible(x)
} else {
stop("could not convert to numeric")
}}
