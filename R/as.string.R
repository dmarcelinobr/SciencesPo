as.string <- function(dataframe)
{
  class.data  <- sapply(dataframe, class)
  factor.vars <- class.data[class.data == "factor"]
  for (colname in names(factor.vars))
  {
    dataframe[,colname] <- as.character(dataframe[,colname])
  }
  return (dataframe)
}