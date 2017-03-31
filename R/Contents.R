#'
#'
#'
#'
`contents` <-function(dfr, descr=FALSE, maxdec=2){
  # Produce and/or print a data frame list of contents
  # Function parameters:
  #   dfr...: A data frame
  #   descr.: To include summary statistics or not
  #   maxdec: Maximum number of decimals for output
  names <- names(dfr)
  n     <- NROW(names)
  col   <- 1:n
  type  <- rep(NA,n)
  zmin  <- type
  zmax  <- type
  zmean <- type
  zmed  <- type
  antal <- rep(NROW(dfr),n)
  unik  <- type
  nmiss <- type
  for (i in 1:n){
    g  <- dfr[,i]
    g2 <- na.omit(g)
    
    type[i]  <- class(g)
    unik[i]  <- NROW(unique(g2))
    nmiss[i] <- sum(is.na(g))
    
    if (descr==TRUE & type[i] != "character") {
      if (type[i] == "factor") g2 <- as.numeric(g2)
      zmin[i]  <- round(min(g2), maxdec)
      zmax[i]  <- round(max(g2), maxdec)
      zmean[i] <- round(mean(g2), maxdec)
      zmed[i]  <- round(median(g2), maxdec)
    }
    rm(g)
  }
  
  pmiss <- round(100*nmiss/antal, 1)
  if (descr==TRUE) {
    ut <-   data.frame(names,type,antal,nmiss,pmiss,unik,zmin,zmean,zmed,zmax)
    names(ut) <-   c("Name","Class","N","NMISS","%MISS","Levels","Min","Mean","Median","Max")
  }
  else {
    ut <- data.frame(names,type,antal,nmiss,pmiss,unik)
    names(ut) <-   c("Name","Class","N","NMISS","%MISS","Levels")
  }
  
  ut$Name <-format(ut$Name, justify="left")
  ut$Class <-format(ut$Class, justify="left")
  writeLines(paste("List of contents"))
  print(ut,justify="left")
}
