rescale <-
function (x, FUN="center"){
  # function to rescale by subtracting the mean and dividing by 2 sd's
  if (!is.numeric(x)){
    x <- as.numeric(factor(x))
    x.obs <- x[!is.na(x)] 
  }
  x.obs <- x[!is.na(x)]
  # for binary cases
  if (length(unique(x.obs))==2){
    if (FUN=="0/1"){
      x <- (x-min(x.obs))/(max(x.obs)-min(x.obs))
      return (x)
    }
    else if (FUN=="-0.5,0.5"){
      return (x-0.5)
    }
    else if (FUN=="center"){
      return (x-mean(x.obs))
    }
    else if (FUN=="full"){
      return ((x-mean(x.obs))/(2*sd(x.obs)))
    }
  }      
  else {
    return ((x-mean(x.obs))/(2*sd(x.obs)))
  }
}
