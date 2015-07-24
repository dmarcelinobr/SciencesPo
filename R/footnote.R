#' @title Generate a footnote for graphs
#'
#' Generate a footnote for graphs
#'
#' @param study The study id.
#' @param adj adj=0 left align the footnote while adj=0.5 or 1 center or right align the text.
#' @param line The line to plot footnote.
#' @param \dots Additional parameters.
#' @examples
#' plot(1:10,10:1)
#' footnote()
#' footnote(study="vote:16")
#' footnote(line=-1, adj=.5, cex=.5)
#'
#' @export
footnote<-function(study=NULL, adj=0, line=3, ...){
  us<-Sys.getenv("username")
  if(us=="") us<-Sys.getenv("USERNAME")
  if(us=="") us<-Sys.getenv("user")
  if(us=="") us<-Sys.getenv("USER")
 if (is.null(study)==FALSE) tmp<-paste(study,"/",us,"/",substr(date(),9,10),substr(date(),5,7),substr(date(),20,24),"/R",R.Version()$major,".",R.Version()$minor,"/",Sys.info()[1],Sys.info()[2],sep="")
  else tmp<-paste(us,"/",substr(date(),9,10),substr(date(),5,7),substr(date(),20,24),"/R",R.Version()$major,".",R.Version()$minor,"/",Sys.info()[1],Sys.info()[2],sep="")
  mtext(tmp, side=1, line=line, adj=adj, ...)
}
