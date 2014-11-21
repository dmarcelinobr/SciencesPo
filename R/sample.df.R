sample.df <- function(df,n,replace=FALSE,vars=NULL) {
  rows <- sample(1:nrow(df),n,replace=replace)
  if (is.null(vars)) df[rows,]
    else {
      vnames <- vars %in% names(df)
      if (all(vnames)) df[rows,vars]
        else stop(paste(paste(vars[vnames],collapse=", ")," name(s) not found in the data frame."),call.=FALSE)
    }
}