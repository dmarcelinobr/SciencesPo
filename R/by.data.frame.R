#' @title  Convert the results of by() to a data.frame.
#'
#' @description  Converts the results of by() to a data.frame if possible,  (reducing dimensionality and adding repetition as necessary)
#'
#'@param x The by object
#'@param row.names Names of the rows. If NULL, function tries guessing them.
#'@param optional Ignored.
#'@param colnames Names of columns
#'@param na.rm Remove NAs or not.
#'@param \dots Pass-alongs.
#'@return A data.frame.
#'@export as.data.frame.by
#'@examples
#'sex    <- factor(rbinom(1:1000, 1, 0.5), labels=c("male" , "female"));
#' age    <- sample(1:100, 1000, rep=TRUE);
#' weight <- factor(rbinom(1:1000, 2, 0.6), labels=c("light", "middle", "heavy"))

#' by(age, sex, mean)
#'
#' @method as.data.frame by
#' @export
as.data.frame.by <- function(x, row.names=NULL, optional=FALSE, colnames=paste("IDX",seq(length(dim(x))),sep="" ), na.rm=TRUE, ... ) {
  num.by.vars <- length(dim(x))
  res <- t(unclass(x))
  if(na.rm) { res <- stats::na.omit(res) }
  colnames(res)[seq(num.by.vars)] <- colnames
  if(!is.null(row.names)) { row.names(res) <- row.names }
  res <- res[ do.call(order,list(res[ , seq(num.by.vars)]) ) , ]
  res
}
NULL
