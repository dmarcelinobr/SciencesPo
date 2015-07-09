#' @encoding UTF-8
#' @title Make a data.frame Rectangular by fill in missing records
#'
#' @description \code{makeRetangular} produces a complete rectangularization table by adding observations with missing data so that all combinations (interactions) of the specified variables exist.
#'
#' @param x a data frame.
#' @param by a vector of at least 2 variables from the data frame. If missing all variables in the data frame will be used.
#' @param fill the value used to fill in, default is \code{fill = 0}.
#'
#' @return a data object of the same class as \code{x}.
#'
#' @examples
#' data <- data.frame(sex=c("female","male","male"),
#' race = c("black","black","white"), y = c(.5,.4,.1), x = c(32,40,53))
#'
#' makeRetangular(data, by=c(sex,race))
#'
#' makeRetangular(data, by=c(sex,race), fill=0)
#'
#' @export
`makeRetangular` <- function(x, by, fill=NA)
{
  if(missing(by)) by=1:ncol(x)
  nl <- as.list(1:ncol(x))
  names(nl) <- names(x)
  vars <- eval(substitute(by), nl, parent.frame())
  xt <- data.frame(table(x[,vars]))
  x0 <- subset(xt, Freq==0)[,-length(xt)]

  if(nrow(x0)==0){
    x
    warning("Nothing to fill")}
  else{
    z <- as.data.frame(x[1:nrow(x0), -vars, drop=FALSE])
    if(dim(z)[2]==1)
      names(z) <- names(x)[-vars]
    z[,] <- fill
    rbind(x, cbind(x0, z))
  }
}
NULL
