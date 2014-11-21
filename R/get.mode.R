get.mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = subset(x, !is.na(x))
  }
  y <- as.factor(x)
  freq <- summary(y)
  mode <- names(freq)[freq[names(freq)] == max(freq)]
  return(as.numeric(mode) )
}
