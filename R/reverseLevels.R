#' @title Reverse the levels of a factor.
#' 
#' @param x a factor whose levels need to be reverse coded.
#' 
#' @examples
#' mylevels <- c('Strongly Disagree', 'Disagree', 'Neither', 'Agree', 'Strongly Agree')
#' 
#' test <- factor(sample(mylevels[1:5], 10, replace=TRUE))
#' 
#' reverseLevels(test)
#' 
#' cbind(test, as.integer(test), as.integer(reverseLevels(test)))
#' 
#' 
#' 
#' @export
reverseLevels <- function(x) {
	if(is.factor(x)) {
		x <- factor(as.character(x), levels=rev(levels(x)), ordered=TRUE)
	} else if(is.data.frame(x)) {
		for(i in seq_along(x)) {
			if(is.factor(x[,i])) {
				x[,i] <- factor(as.character(x[,i]), levels=rev(levels(x[,i])), ordered=TRUE)
			} else {
				warning(paste0('Column ', i, ' is not a factor.'))
			}
		}
	} else {
		stop(paste0('Unsupported format: ', class(x)))
	}
	return(x)
}
