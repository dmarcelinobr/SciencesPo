#' @encoding UTF-8
#' @title Unity-based normalization
#'
#' @description The function normalizes as feature scaling \code{min - max}, or unity-based normalization. Typically used to bring all values into the range [0,1], this may also be generalized to restrict the range of values to any arbitrary points \code{a}  and  \code{b}, using: \deqn{X' = a + \frac{(x - x_{min})(b - a)}{(x_{max} - x_{min})}}.
#'
#' @param x is a vector to be normalized.
#' @param range is a numeric vector of length 2 as \code{0:1} for min and max values, default is \code{c(0,1)}.
#' @param domain a numeric vector of length 2.
#' @param \dots further arguments passed to or used by other methods.
#' @return Normalized values in an object of the same class as \code{x}.
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @seealso  \code{\link{svTransform}}.
#'
#' @examples
#' x <- sample(10)
#' normalize(x, range=(0:1))
#' normalize(x)
#'
#' char = LETTERS
#' normalize(char, range=(0:1), domain=range(w, na.rm=TRUE))
#'
#' @keywords Rescaling
#' @keywords Normalization
#' @seealso  \code{\link{scale}}.
#'
#' @export
`normalize` <- function(x, range, domain, ...) {
  UseMethod("normalize")
}

#' @rdname normalize
#' @export
`normalize.factor` <- function(x, range, domain=range(1:nlevels(x)), ...) {
  width <- diff(range)
  n <- length(levels(x)) - 1
  range[1]  - 1/n + width * as.numeric(x) / n
}

#' @rdname normalize
#' @param center Enables values be centered, default is \code{TRUE}.
#' @param scale Enables values be scaled, default is \code{TRUE}.
#'
#' @note When center and scale are both set \code{TRUE}, the resulting values are.
#' @export
`normalize.numeric` <- function(x, center=TRUE, scale=TRUE, range=c(0,1), domain=range(x, na.rm=TRUE), ...) {
  range_width  <- diff(range)
  domain_width <- diff(domain)
  range[1] + range_width * (x - min(x)) / domain_width
}

#' @rdname normalize
#' @export
`normalize.default` <- function(x, range=c(0,1), domain, ...) {
  normalize( as.numeric(x), range=range, domain, ... )
}

#' @rdname normalize
#' @export
`normalize.character` <- function(x, range=c(0,1), domain, ...) {
  normalize( as.factor(x), range=range, domain=domain)
}
NULL

