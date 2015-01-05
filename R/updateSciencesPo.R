#' @title Update SciencesPo
#' 
#' 
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @importFrom devtools install_github
#' @export
updateSciencesPo<-function(){
  require(devtools)
  install_github("danielmarcelino/SciencesPo")
  require(SciencesPo)
}
