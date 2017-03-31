#' @encoding UTF-8
#' @title Unity-based normalization
#'
#' @description Normalizes as feature scaling \code{min - max}, or unity-based
#' normalization typically used to bring the values into the range [0,1].
#' Other methods are also available, including scoring, centering,
#' and Smithson and Verkuilen (2006) method of dependent variable transformation.
#'
#' @param x is a vector to be normalized.
#' @param method A string for the method used for normalization. Default is \code{method = "range"}, which coerces values into [0,1] range. See details for other implemented methods.
#' @param \dots Additional arguements (currently ignored).
#'
#' @return Normalized values in an object of the same class as \code{x}.
#' @details The following methods are available:
#' \itemize{
#' \item {"range"}{Ranging is done by coercing \code{x} values into [0,1] range.
#' However, this may be generalized to follow the range of other arbitrary values
#' using  \code{a} and \code{b}: \deqn{X' = a + \frac{(x - x_{min})(b - a)}{(x_{max} - x_{min})}}}.
#' \item {"scale"}{Scaling is done by dividing (centered) values of \code{x} by
#' their standard deviations.}
#' \item {"center"}{Centering is done by subtracting the means (omitting NAs)
#' of \code{x} from its observed value.}
#' \item {"z-score"}{Scoring is done by dividing the values of \code{x}
#' from their root mean square.}
#' \item {"SV"}{Transform a dependent variable in [0,1] rather than (0, 1) to beta regression as suggested by Smithson and Verkuilen (2006).}
#' }
#'
#' @references
#' Smithson M, Verkuilen J (2006) A Better Lemon Squeezer? Maximum-Likelihood
#' Regression with Beta-Distributed Dependent Variables.
#' \emph{Psychological Methods}, 11(1), 54-71.
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @seealso \code{\link{scale}}.
#'
#' @examples
#'
#' x <- sample(10)
#' (y = Normalize(x) )
#'
#' # equivalently to
#' (x-min(x))/(max(x)-min(x))
#'
#' # Smithson and Verkuilen approach
#' (y = Normalize(x, method="SV") )
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
#' @keywords Modelling
#'
#' @export
`Normalize` <- function(x, method = "range", ...) UseMethod("Normalize")

#' @export
`Normalize.default` <- function(x, method = "range", ...){
  method = .Match(arg = method, choices = c("range", "scale", "center", "z-score", "SV"))
  n <- length(x)
  mat <- as.matrix(x)
  if(method=="range"){
  min_attr <- apply(mat, 2, min, na.rm = TRUE)
  max_attr <- apply(mat, 2, max, na.rm = TRUE)
  mat <- sweep(mat, 2, min_attr, FUN = "-")
  ans <- sweep(mat, 2,  max_attr-min_attr, "/")
  attr(ans, 'normalized:min') = min_attr
  attr(ans, 'normalized:max') = max_attr
  invisible(ans)
  }
  else if(method=="sv"){
  y <- Normalize(x);
  ans <- (y * (n-1) + 0.5)/n;
  invisible(ans)
  }
  else if(method=="center"){
    invisible(ans)
  }
  else if(method=="z-score"||method=="scale"){
    invisible(ans)
  }
  else if (!is.numeric(resul <- x))
    warning("Data not numeric, normalization not applicable")
  else stop("Unknown input method")
}
NULL


# check that we get mean of 0 and sd of 1
#colMeans(scaled.dat)  # faster version of apply(scaled.dat, 2, mean)
#apply(scaled.dat, 2, sd)


# Convert data to Normal Scores with the same Mean and SD.  This reshapes data to conform to a Normal Distribution. It is not converting to z-scores (i.e., it is not standardizing data)

#return \value{A numeric with the same Mean and SD as x, but without skew or kurtosis}
#uniformize <- function (x)
#{
#  x <- rank(x,
#            na.last = "keep",
#            ties.method = "average")
#  n <- sum(!is.na(x))
#  x / (n + 1)
#}

#return( qnorm(uniformize(x), mean=mean(x, na.rm=TRUE), sd=sd(x, na.rm=TRUE) ) )
#}

