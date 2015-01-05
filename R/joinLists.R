#' @title Join a list of data frames
#' 
#' @description Recursively join data frames 
#' 
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' 
#' @param x A list of data frames
#' @param \dots Arguments passed onto merge
#' 
#' @examples
#' mtcars$cars <- row.names(mtcars)
#' df1 <- mtcars[,c(1:2,12)]
#' df2 <- mtcars[,c(3:4,12)]
#' df3 <- mtcars[,c(5:6,12)]
#' joinLists(x=list(df1, df2, df3), by="cars")

#' @export
joinLists <-
function(x, ...)
{
	dfs1 <- x[[1]]
	dfs2 <- x[-1]
	for(i in 1:length(dfs2)){
		dfs1 <- merge(dfs1, dfs2[[i]], all = TRUE, sort = FALSE, ...)
	}
	return( dfs1 )
}
