KFpredict<- function(mean1, var1, mean2, var2){
  new_mean = mean1 + mean2
  new_var = var1 + var2
  ans <- cbind(new_mean,new_var)
  return(ans)
}
