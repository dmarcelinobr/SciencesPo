#' @title Slice a vector
#' 
#' @description Break up a vector by certain N sized chunks
#' 
#' @param x A numeric vector
#' @param by The number by which to split the vector
#' @param pattern The number of blocks
#' 
#' @examples
#' x <- seq(1:15)
#' slice(x, by = 2, pattern = 4)
#'  
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' 
#' @export
slice <-
function(x, by = 2, pattern  = NULL) {
	if(is.null(pattern)){	
		starts <- seq(1, length(x), by)
		tt <- lapply(starts, function(y) x[y:(y + (by - 1))])
		lapply(tt, function(x) x[!is.na(x)])
	} else
	{
		splitby <- round(length(x)/pattern)+1
		starts <- seq(1, length(x), splitby)
		tt <- lapply(starts, function(y) x[y:(y + (splitby - 1))])
		lapply(tt, function(x) x[!is.na(x)])
	}
}
