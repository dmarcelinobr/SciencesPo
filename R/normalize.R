#' @encoding UTF-8
#' @title Unity-based normalization
#'
#' @description Normalizes as feature scaling \code{min - max}, or unity-based normalization typically used to bring the values into the range [0,1].
#'
#' @param x is a vector to be normalized.
#' @param method A string for the method used for normalization. Default is \code{method = "range"}, which brings the values into the range [0,1]. See details for other implemented methods.
#'
#' @return Normalized values in an object of the same class as \code{x}.
#' @details This approach may also be generalized to restrict the range of values to any arbitrary values \code{a}  and  \code{b}, using: \deqn{X' = a + \frac{(x - x_{min})(b - a)}{(x_{max} - x_{min})}}.
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @seealso  \code{\link{svTransform}}, \code{\link{scale}}.
#'
#' @examples
#'
#' x <- sample(10)
#' normalize(x)
#'
#' # equivalently to
#' (x-min(x))/(max(x)-min(x))
#'
#' @keywords Rescaling
#' @keywords Transformation
#'
#' @export normalize
#' @docType methods
#' @rdname normalize-methods
#' @aliases normalise
`normalize`<-setClass("normalize", representation(x = "numeric", method="character"))
setGeneric("normalize", def=function(x, method = "range"){
  standardGeneric("normalize")
})
#' @rdname normalize-methods
setMethod(f="normalize", definition=function(x, method = "range"){
  method = .Match(arg = method, choices = c("range", "center", "Z-score", "z-score", "scale"))
  mat <- as.matrix(x)
  if(method=="range"){
  min_attr = apply(mat, 2, min)
  max_attr = apply(mat, 2, max)
  mat <- sweep(mat, 2, min_attr, FUN="-")
  ans = sweep(mat, 2,  max_attr-min_attr, "/")
  attr(ans, 'normalized:min') = min_attr
  attr(ans, 'normalized:max') = max_attr
  return(ans)
  }
  else if(method=="center"){
  }
  else if(method=="Z-score"||method=="z-score"||method=="scale"){
  }
  else if (!is.numeric(resul <- x))
    warning("Data not numeric, normalization not applicable")
  else stop("Unknown input method")
})
NULL




# check that we get mean of 0 and sd of 1
#colMeans(scaled.dat)  # faster version of apply(scaled.dat, 2, mean)
#apply(scaled.dat, 2, sd)


#`normalize.factor` <- function(x, range, domain=range(1:nlevels(x)), ...) {
#  width <- diff(range)
#  n <- length(levels(x)) - 1
#  range[1]  - 1/n + width * as.numeric(x) / n
#}



##' @title Generic function for obtaining scaled coefficients
##'
##' @description Given an object of class lm, glm, or lda, this function will first standardize the variables, then run the model again. The resulting
##' coefficients will be standardized betas.
##' @title Standardize coefficients
##' @param object an object resulting from glm, lm, or lda
##' @param scale.response should the response variable be scaled as well? (Usually not for glm or lda).
##' @return an object of the same class as the one outputted
##' @export
##' @author Daniel Marcelino
##' @examples
##' 		#### create random data with different means and variances
##' d = data.frame(matrix(rnorm(5*50, c(10,5,14,100, 33), c(3,5,4,3,5)), nrow=50, byrow=TRUE))
##' names(d) = LETTERS[1:5]
##' g = lm(C~B + A + D + E, data=d)
##' scaleB(g, TRUE)
`scaleB` = function(object, scale.response=F){
  vars = row.names(attr(terms(object), "factors"))
  if (scale.response){
    part1 = paste("scale(", vars[1], ")~", sep="")
  } else {
    part1 = paste("(", vars[1], ")~", sep="")
  }
  new.form = formula(paste(part1,paste("scale(", vars[-1], ")", collapse="+", sep=""), sep=""))
  n.object = update(object, new.form, evaluate=FALSE)
  n.object = eval.parent(n.object)
  return(n.object)
}
NULL


