#' Rescale a numeric vector
#'
#' \code{rescale} returns a rescaled numeric vector.
#'
#' \code{rescale} returns a rescaled version of the numeric vector \code{x}.
#'   \code{NA} values in \code{x} are ignored during the rescaling process but
#'   are preserved in the output.
#'
#' \code{rescale} is designed to be readable from the function call.
#'   For example:
#' \itemize{
#'   \item \code{rescale(x, "normal", mean=0, sd=1)} can be read as
#'     "\strong{Rescale} \strong{x} using a \strong{normal}-style
#'     transformation with \strong{mean 0} and \strong{standard deviation 1}".
#'   \item \code{rescale(x, "minmax", min=0, max=1)} can be read as
#'     "\strong{Rescale} \strong{x} using a \strong{min/max}-style
#'     transformation with \strong{min 0} and \strong{max 1}".
#' }
#'
#' The arguments \code{mean}, \code{sd}, \code{min}, and \code{max} are used
#'   based on \code{method}.  \code{rescale} offers a couple of different options
#'   for \code{method}:
#'
#' \itemize{
#'   \item \strong{normal}: The default option that rescales \code{x} using
#'     a normal-style transformation into a distribution with mean \code{mean}
#'     and standard deviation \code{sd}.  The default values for this rescaling
#'     are \code{mean=0} and \code{sd=1}.
#'
#'     The explicit formula for this transformation is:
#'
#'     \deqn{(x - \mu_x)/(\sigma_x) * \sigma + \mu}
#'
#'     where \eqn{\mu_x} is the sample mean of \code{x}, \eqn{\sigma_x}
#'     is the sample standard deviation of \code{x}, \eqn{\mu} is the desired
#'     mean, and \eqn{\sigma} is the desired standard deviation.
#'
#'   \item \strong{minmax}: rescales \code{x} using a min/max-style
#'     transformation into a distribution with minimum \code{min} and maximum
#'     \code{max}.  The default values for this rescaling are \code{min=0}
#'     and \code{max=1}.
#'
#'     The explicit formula for this transformation is:
#'
#'     \deqn{((x - min_x)/(max_x - min_x)) * (max - min) + min}
#'
#'     where \eqn{min_x} is the sample minimum of \code{x}, \eqn{max_x}
#'     is the sample maximum of \code{x}, \eqn{min} is the desired minimum,
#'     and \eqn{max} is the desired maximum.
#' }
#'
#' @param x A numeric vector.
#' @param method A character string indicating the desired method of rescaling
#'   with \code{"normal"} being the default.  This must be (an abbreviation of)
#'   one of the strings \code{"normal"} or \code{"minmax"}.  See Details
#'   for more information.
#' @param mean The desired mean value for normal-style scaling.
#'   Used only when \code{method="normal"}.
#' @param sd The desired standard deviation value for normal-style scaling.
#'   Used only when \code{method="normal"}.
#' @param min The desired minimum value for min/max-style scaling.
#'   Used only when \code{method="minmax"}.
#' @param max The desired maximum value for min/max-style scaling.
#'   Used only when \code{method="minmax"}.
#' @return The output of \code{rescale} is a rescaled numeric vector with
#'   the same length as \code{x}.
#' @export
#' @examples
#' set.seed(1337)
#' x <- rnorm(1e4)
#' summary(x)
#'
#' #####
#' # Common use cases
#' #
#'
#' # I want to rescale to a standard normal distribution!
#' x_normal <- rescale(x)
#' summary(x_normal)
#' mean(x_normal); sd(x_normal)
#'
#' # I want to rescale to be between 0 and 1!
#' x_minmax <- rescale(x, "minmax")
#' summary(x_minmax)
#' min(x_minmax); max(x_minmax)
#'
#' # I want to rescale to be between 300 and 850! (Weird but some credit scores do it!)
#' x_credit <- rescale(x, "minmax", min=300, max=850)
#' summary(x_credit)
#' min(x_credit); max(x_credit)

rescale <- function(x, method=c("normal", "minmax"), mean=0, sd=1, min=0, max=1) {
  # Check x
  if (missing(x)) {
    stop("Please provide a vector x to rescale", call.=FALSE)
  } else if (!is.numeric(x)) {
    stop("x must be a numeric vector", call.=FALSE)
  }

  # Check method
  method <- match.arg(method)

  # Check mean/sd if method="normal"
  if (method=="normal") {
    if (!is.numeric(mean)) {
      stop("mean must be a numeric value", call.=FALSE)
    } else if (length(mean) != 1) {
      stop("mean must be a single value", call.=FALSE)
    } else if (!is.numeric(sd) || sd <= 0) {
      stop("sd must be a positive numeric value", call.=FALSE)
    } else if (length(sd) != 1) {
      stop("sd must be a single value", call.=FALSE)
    }
  }

  # Check min/max if method="minmax"
  if (method=="minmax") {
    if (!is.numeric(min)) {
      stop("min must be a numeric value", call.=FALSE)
    } else if (length(min) != 1) {
      stop("min must be a single value", call.=FALSE)
    } else if (!is.numeric(max)) {
      stop("max must be a numeric value", call.=FALSE)
    } else if (length(max) != 1) {
      stop("max must be a single value", call.=FALSE)
    } else if (min > max) {
      stop("min must be less than or equal to max", call.=FALSE)
    }
  }

  # Rescale
  if (method=="normal") {
    mean_x <- mean(x, na.rm=TRUE)
    sd_x <- sd(x, na.rm=TRUE)
    (x - mean_x) / (sd_x) * sd + mean
  } else {
    min_x <- min(x, na.rm=TRUE)
    max_x <- max(x, na.rm=TRUE)
    (x - min_x) / (max_x - min_x) * (max - min) + min
  }
}
