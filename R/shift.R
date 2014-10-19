shift <-
function(x,delta=NA){
  stopifnot(is.numeric(delta))
  stopifnot(is.numeric(x))
  
  if (length(delta)>1)
    return(sapply(delta,shift, x=x))
  
  output<-NULL
  abs.delta=abs(delta)
  if (delta > 0 )
    output<-c(tail(x,-abs.delta),rep(NA,abs.delta))
  else if (delta < 0 )
    output<-c(rep(NA,abs.delta), head(x,-abs.delta))
  else 
    output <- x
  return(output)
}
