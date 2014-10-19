anonymize <-
function(x, keep.names=TRUE){
  truenames <- names(x)
  if(length(x)>26){
   # letters <-replicate(floor(length(x)/26),{letters <-c(LETTERS, paste(LETTERS, LETTERS, sep=""))})
  }
  names(x)<-paste(sample(letters[1:length(x)]))
  level.x<-function(x){
    level.obs<-function(i){
      if(class(x[,i])=="factor" | class(x[,i])=="character"){
        var <-paste(names(x)[i],as.numeric(as.factor(x[,i])), sep="")
        }else if(is.numeric(x[,i])){
          var <-x[,i]- mean(x[,i], na.rm=T)}else{var<-x[,i]}
      return(var)
    }
    x <- data.frame(sapply(seq_along(x), level.obs))
    
    if(keep.names==TRUE){
      names(x) <- truenames 
    }else{
      names(x) <- names(x)
    }
    return(x)
  }
  x <-level.x(x)
  return(x)
}
