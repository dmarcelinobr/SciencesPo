center_by <- function(x,group) {
	if(nargs() > 1 && is.numeric(x) && is.factor(group)) {
    return(x-tapply(x,group,mean,na.rm=T)[group])
}
	else {
 cat("Usage: center_by(x,group)\n")
cat("\twhere x is a numeric object and group is a factor\n")  
}
}