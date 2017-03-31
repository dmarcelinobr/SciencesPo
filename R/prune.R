#' Prune a numeric vector
#'
#' \code{prune} returns a pruned numeric vector.
#'
#' \code{prune} returns a pruned version of the numeric vector \code{x}.
#'   \code{NA} values in \code{x} are ignored during the pruning process
#'   but are preserved in the output.  \code{prune} will do one-sided pruning
#'   if only one \code{low} or \code{high} argument is provided
#'   (e.g. \code{prune(x, low=-1)} will prune \code{x} at a lower value of -1).
#'
#' \code{prune} is designed to be readable from the function call.  For example:
#' \itemize{
#'   \item \code{prune(x, "value", low=-1, high=1)} can be read as
#'     "\strong{prune} \strong{x} at \strong{-1} and \strong{1}".
#'   \item \code{prune(x, "percentile", low=.05, high=.95)} can be read as
#'     "\strong{prune} \strong{x} at the \strong{5}th and \strong{95}th percentiles".
#' }
#'
#' The arguments \code{low} and \code{high} are used based on \code{method}.
#'   \code{prune} offers several different options for \code{method}:
#'
#' \itemize{
#'   \item \strong{value}: \code{low} and \code{high} are used as raw values
#'     (e.g. .05 is the value .05).
#'   \item \strong{percentile}: \code{low} and \code{high} are used as percentiles
#'     (e.g. .05 is 5th percentile).
#' }
#'
#' @param x A numeric vector.
#' @param method A character string indicating the desired method of pruning
#'   with \code{"value"} being the default.  This must be (an abbreviation of)
#'   one of the strings \code{"value"} or \code{"percentile"}.
#'   See Details for more information.
#' @param low The lower value/percentile for pruning  See Details for more information.
#' @param high The upper value/percentile for pruning  See Details for more information.
#' @param replace Either \code{NULL} ("rounds" values according to the low and high arguments),
#'   \code{NA},
#'   or a single value that will replace the pruned values.  The default is NULL.
#' @return The output of \code{prune} is a pruned numeric vector with the same
#'   length as \code{x}.
#' @export
#' @examples
#' set.seed(51)
#' x <- rnorm(1e4)
#' summary(x)
#'
#' # pruning at the values -1 and 1!
#' x_val <- prune(x, low=-1, high=1)
#' summary(x_val)
#'
#' # pruning at 5th and 95th percentiles!
#' x_per <- prune(x, "percentile", low=.05, high=.95)
#' summary(x_per)
#'
#' # pruning values above the 95th percentile!
#' x_hi <- prune(x, "percentile", high=.95)
#' summary(x_hi)
#'
#' # pruning values converted to NAs!
#' x_NA <- prune(x, "percentile", low=.05, high=.95, replace=NA)
#' summary(x_NA)
#'
`prune` <- function(x, method=c("value", "percentile"), low=NULL, high=NULL, replace=NULL) {
  # Check x
  if (missing(x)) {
    stop("Please provide a vector x to prune", call.=FALSE)
  } else if (!is.numeric(x)) {
    stop("x must be a numeric vector", call.=FALSE)
  }

  # Check method
  method <- match.arg(method)

  # Check that at least one low/high value provided
  if (is.null(low) && is.null(high)) {
    stop("Please provide at least one low or high value")
  }

  # Check low
  if (!is.null(low)) {
    if (!is.numeric(low)) {
      stop("low must be a numeric value if specified", call.=FALSE)
    } else if (length(low) != 1) {
      stop("low must be a single value if specified", call.=FALSE)
    }
  }

  # Check high
  if (!is.null(high)) {
    if (!is.numeric(high)) {
      stop("high must be a numeric value if specified", call.=FALSE)
    } else if (length(high) != 1) {
      stop("high must be a single value if specified", call.=FALSE)
    }
  }

  # Check low <= high
  if (!is.null(low) && !is.null(high) && high < low) {
    stop("low must be less than or equal to the high", call.=FALSE)
  }

  # Check replace
  if (!is.null(replace)) {
    if (!is.numeric(replace) && !is.na(replace)) {
      stop("replace must be a numeric value or NA if specified", call.=FALSE)
    } else if (length(replace) != 1) {
      stop("replace must be a single value if specified", call.=FALSE)
    }
  }

  # Check low/high in [0,1] if method="percentile"
  if (method=="percentile") {
    if (!is.null(low) && (low < 0 | low > 1)) {
      stop("low must be in the range 0 <= low <= 1 for method='percentile'", call.=FALSE)
    }
    if (!is.null(high) && (high < 0 | high > 1)) {
      stop("high must be in the range 0 <= high <= 1 for method='percentile'", call.=FALSE)
    }
  }

  # Derive percentiles if method="percentile"
  if (method=="percentile") {
    if (!is.null(low)) {
      low <- quantile(x, prob=low, type=8, na.rm=TRUE)
    }
    if (!is.null(high)) {
      high <- quantile(x, prob=high, type=8, na.rm=TRUE)
    }
  }

  # prune
  if (!is.null(low)) {
    x[x < low] <- ifelse(!is.null(replace), replace, low)
  }
  if (!is.null(high)) {
    x[x > high] <- ifelse(!is.null(replace), replace, high)
  }
  x
}
NULL
