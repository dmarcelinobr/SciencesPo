join.lists <-
function(dfs, ...)
{
	dfs1 <- dfs[[1]]
	dfs2 <- dfs[-1]
	for(i in 1:length(dfs2)){
		dfs1 <- merge(dfs1, dfs2[[i]], all = TRUE, sort = FALSE, ...)
	}
	return( dfs1 )
}
