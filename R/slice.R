slice <-
function(input, by = 2, pattern  = NULL) {
	if(is.null(pattern)){	
		starts <- seq(1, length(input), by)
		tt <- lapply(starts, function(y) input[y:(y + (by - 1))])
		lapply(tt, function(x) x[!is.na(x)])
	} else
	{
		splitby <- round(length(input)/pattern)+1
		starts <- seq(1, length(input), splitby)
		tt <- lapply(starts, function(y) input[y:(y + (splitby - 1))])
		lapply(tt, function(x) x[!is.na(x)])
	}
}
