#' @title Clears objects and environments
#'
#' @description Clears all objects including visible and hidden environments.
#'
#'
#'
#' @export
#'
`clear` <- function(){
max.fails <- 500
.GetNondefaultLoadedPackages <- function() {
  setdiff(loadedNamespaces(), c("base", "tools", "grid", "lattice", "tcltk", "ggplot2", options()$defaultPackages))
}

fail.counter <- 0
while( length(pkgs.to.remove <- .GetNondefaultLoadedPackages()) > 0 ) {
  res <- tryCatch( unloadNamespace(sample(pkgs.to.remove, 1)),
                   error=function(e) e,
                   warning=function(w) w)
  if( !is.null(res) ) fail.counter <- fail.counter + 1
  if(fail.counter >= max.fails) break
}

if(fail.counter >= max.fails) {
  cat("Unable to remove all package environments from the search() path.",
      "You may want to restart R to guarantee a clean session.\n",
      sep="\n")
} else {
  cat("All packages were unloaded.\n\n")
  rm(list=ls(all.names=TRUE, envir=.GlobalEnv), envir=.GlobalEnv)
  cat("All objects were deleted, including hidden package environments.\n")
}
}
