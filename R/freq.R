#' @title Frequency table
#'
#' @description Simulating the FREQ procedure of SPSS.
#'
#'  @param x The data.frame
#'  @param \dots Further arguments.
#'
#' @examples
#'
#' freq2(titanic$CLASS)
#'
#' @export
`freq` <- function(x,...)
{
  nmiss=sum(is.na(x))
  fsum=summary(factor(x))
  ftab=cbind(fsum,100*fsum/sum(fsum))
  if (nmiss==0)
  {
    ftab=cbind(ftab,100*cumsum(fsum)/sum(fsum))
    colnames(ftab)=c("Frequency"," Valid Percent"," Cum Percent")
    ftab[,2] <- round(ftab[,2],2)
    ftab[,3] <- round(ftab[,3],2)
    print(ftab)
  }
  else
  {
    ftab=cbind(ftab,100*fsum/sum(fsum[1:(length(fsum)-1)]),100*cumsum(fsum)/sum(fsum[1:(length(fsum)-1)]))
    ftab[length(fsum),3:4]=NA
    ftab[,2] <- round(ftab[,2],2)
    ftab[,3] <- round(ftab[,3],2)
    ftab[,4] <- round(ftab[,4],2)
    cat("\n")
    colnames(ftab)=c("Frequency","   Percent"," Valid Percent"," Cum Percent")
    if (dim(ftab)[1]==length(levels(x)))
    {
      rownames(ftab)[1:length(levels(factor(x)))]=levels(factor(x))
    }
    print(ftab)
  }
  cat("-----------------------------------------------------\n")
  cat("Total",rep(" ",8-trunc(log10(sum(fsum)))),sum(fsum),"\n",sep="")
  cat("\n")
  if (length(attributes(x)$class) != 0)
  {
    if ("factor" %in% attributes(x)$class)
    {
      cat("Warning: Statistics may not be meaningful for factors!\n\n")
    }
  }
  s1=cbind(mean(as.numeric(x),na.rm=TRUE),sd(as.numeric(x),na.rm=TRUE))
  rownames(s1)=" "
  colnames(s1)=c("       Mean","      Std dev")
  print(s1)
  s2=cbind(min(as.numeric(x),na.rm=TRUE),max(as.numeric(x),na.rm=TRUE))
  rownames(s2)=" "
  colnames(s2)=c("    Minimum","      Maximum")
  print(s2)
  s3=cbind(sum(!is.na(x)),nmiss)
  rownames(s3)=" "
  colnames(s3)=c("Valid cases","Missing cases")
  print(s3)
  cat("\n")
}
NULL
