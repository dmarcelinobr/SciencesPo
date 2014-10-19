psum <-
function(..., na.rm=FALSE) { 
x <- list(...)
 rowSums(matrix(unlist(x), ncol=length(x)), na.rm=na.rm)
 }
