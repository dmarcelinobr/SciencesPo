KFupdate <- function(mean1, var1, mean2, var2){
  new_mean = (var2 * mean1 + var1 * mean2) / (var1 + var2)
new_var = 1 / (1/var1 + 1/var2)
ans <- cbind(new_mean,new_var)
return(ans)
}
