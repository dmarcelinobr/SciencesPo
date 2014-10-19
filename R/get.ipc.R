get.ipc <-
function(n, mag=NULL){ 
if(mag < 1) stop("The district magnitude must be at least 1")
.ipc <- ((2 * n) / (mag) - 1)/100
  return(.ipc)
}
