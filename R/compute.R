#' @encoding UTF-8
#' @title Aggregate a numeric variable
#'
#' @param formula The variable versus factor(s) to be computed (y~factor+factor).
#' @param data The data object.
#' @param FUN The function statistic to be calculated.
#' @examples
#' # data:
#' df=data.frame(group=sample(letters,100, TRUE),y=sample(100) )
#'
#' #functions:
#' FUNS <- function(x) c(N=nrow(x), mean=round(mean(x),0),
#' sd=round(sd(x), 0), min=round(min(x),0),
#' max=round(max(x),0))
#'
#' # Do the computation
#' compute(y~group, data=df, FUN=FUNS)
#' @export
compute <- function(formula, data=.data, FUN){
  if(class(FUN)=="list"){
    f <- function(x) sapply(FUN, function(fun) fun(x))
  }else{f <- FUN}
  temp <- aggregate(formula, data, f)
  out <- data.frame(temp[,-ncol(temp)], temp[,ncol(temp)])
  colnames(out)[1] <- colnames(temp)[1]
  return(out)
}
NULL
