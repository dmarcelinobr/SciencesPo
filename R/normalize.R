#' @encoding UTF-8
#' @title Unity-based normalization
#'
#' @description Normalizes as feature scaling \code{min - max}, or unity-based normalization typically used to bring the values into the range [0,1].
#'
#' @param x is a vector to be normalized.
#' @param method A string for the method used for normalization. Default is \code{method = "range"}, which brings the values into the range [0,1]. See details for other implemented methods.
#' @param \dots Additional arguements (currently ignored).
#'
#' @return Normalized values in an object of the same class as \code{x}.
#' @details This approach may also be generalized to restrict the range of
#'  values to any arbitrary values \code{a}  and  \code{b}, using:
#'  \deqn{X' = a + \frac{(x - x_{min})(b - a)}{(x_{max} - x_{min})}}.
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
#' # look at what happens to the correlation of two independent variables and their "interaction"
#' # With non-centered interactions:
#' a = rnorm(10000,20,2)
#' b = rnorm(10000,10,2)
#' cor(a,b)
#' cor(a,a*b)
#'
#' # With centered interactions:
#' c = a - 20
#' d = b - 10
#' cor(c,c*d)
#'
#' @keywords Rescaling
#' @keywords Transformation
#'
#' @export normalize
#' @rdname normalize
`normalize` <- function(x, method = "range", ...) UseMethod("normalize")

#' @rdname normalize
normalize <- function(x, method = "range", ...){
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
}
NULL


# check that we get mean of 0 and sd of 1
#colMeans(scaled.dat)  # faster version of apply(scaled.dat, 2, mean)
#apply(scaled.dat, 2, sd)


#' @encoding UTF-8
#' @title Transform dependent variable
#' @description Simple function to transform a dependent variable that in [0,1] rather than (0, 1) to beta regression. Suggested by Smithson & Verkuilen (2006).
#'
#' @param y the dependent variable in [0, 1] interval.
#' @references
#' Smithson M, Verkuilen J (2006) A Better Lemon Squeezer? Maximum-Likelihood Regression with Beta-Distributed Dependent Variables. \emph{Psychological Methods}, 11(1), 54-71.
#'
#' @seealso  \code{\link{normalize}}.
#' @examples
#'  x <- sample(10); x;
#'  y <- normalize(x); y;
#'  svTransform(y)
#' @export
`svTransform` <- function(y)
{
  n <- length(y)
  trans <- (y * (n-1) + 0.5)/n
  return(trans)
}### end -- svTransform function
NULL


