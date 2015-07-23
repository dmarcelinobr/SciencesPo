#' @encoding UTF-8
#' @title View random n elements of an object.
#'
#' @description Provide a sly view of the data by randomly drawn observations, instead of showing only the first \code{head()} or the last \code{tail()} rows of an object, though it is also possible to constrain for the first/last elements.
#'
#' @param data A matrix or data.frame object
#' @param n The number of rows to be shown
#' @param random If TRUE, observartions are sampled.
#' @param tail If TRUE, will show n lines from tail.
#' @param print.console If TRUE, dada will be printed on console.
#' @param \dots Some additional parameters.
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#'
#'
#' @keywords Tables
#' @examples
#' view(titanic)
#'
#' @export
`view` <- function (data=.data, n=10, random=TRUE, tail=FALSE, print.console=TRUE, ...) {
  getn=function(n,N,tail,random,...) {
    n=min(n,N)
    if (random) return(sample(1:N,n,...))
    n1=ifelse(tail,N-n+1,1); n2=ifelse(tail,N,n)
    return(n1:n2) }
  showVec=function(data,n,tail,random,...){
    N=length(data); if (N==0) return("empty vector")
    v.vec=data[getn(n,N,tail,random,...)]
    return(v.vec) }
  showTab=function(data,n,tail,random,...){
    N=nrow(data); if (N==0) return("empty table")
    mess = paste(c("v.tab=data[getn(n,N,tail,random,...)",rep(",",length(dim(data))-1),"]"),collapse="")
    eval(parse(text=mess))
    return(v.tab) }
  showLis=function(data,n,tail,random,print.console,...){
    nL=length(data); if (nL==0) return("empty list")
    v.lis=list()
    if (is.null(names(data))) ii=1:nL else ii=names(data)
    for (i in 1:nL) {
      iobj=data[[i]]
      v.lis = c(v.lis,list(view(iobj,n,tail,random,print.console=FALSE,...)))
    }
    names(v.lis)=ii; return(v.lis) }
  showAll=function(data){
    return(data) }
  # End Subfunction------------------------------
  if (n==0) return("nada")
  n=abs(n) # coerce to positive
  if (is.data.frame(data) || is.matrix(data) || is.array(data))
    viewed=showTab(data,n,tail,random,...)
  else if (is.list(data))
    viewed=showLis(data,n,tail,random,print.console,...)
  else if (is.vector(data) || is.integer(data) || is.numeric(data) || is.character(data))
    viewed=showVec(data,n,tail,random,...)
  else viewed=showAll(data)
  if (print.console) print(viewed)
  invisible(viewed)

  nr <- nrow(data);
  nc <- ncol(data);
  message(paste0("A kind of ", '[', nr, " x ", nc, ']', " data object.", sep=""));
}
NULL
