#' @title Detach All Data From the Memory
#'
#' @description Detach all data from the memory. 
#'
#' @examples
#'detachAll()
#'
#' @export
detachAll <-
  function () 
  {
    pos.to.detach <- (1:length(search()))[substring(search(), 
    first = 1, last = 8) != "package:" & search() != ".GlobalEnv" & 
  search() != "Autoloads" & search() != "CheckExEnv" & search() != "tools:rstudio" & search() != "TempEnv"]
    for (i in 1:length(pos.to.detach)) {
      if (length(pos.to.detach) > 0) {
        detach(pos = pos.to.detach[1])
        pos.to.detach <- (1:length(search()))[substring(search(), 
 first = 1, last = 8) != "package:" & search() !=".GlobalEnv" & search() != "Autoloads" & search() != "CheckExEnv" & search() != "tools:rstudio" & search() != "TempEnv"]
      }
    }
  }