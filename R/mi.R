#' @title Identify Columns with at least one NA value
#' 
#' @param x a \code{data.frame}
#' 
#' @return A message indicating whether any column in \code{x} has missing data.
#' 
#' @examples
#' data(ssex)
#' mi(ssex)
#' 
#' @export
mi <- function(x)  {
for (i in 1:ncol(x))  { 
if (sum(is.na(x[,i])) > 0 ) { 
print(paste("column",i,"has missing data")) 
mean.col <- mean(x[,i], na.rm=T)
for (j in 1:nrow(x))  {
if (is.na(x[j,i]) ==T)
		  x[j,i] <- mean.col
		  } 
	  }
  }
return(x)
 }
NULL
 