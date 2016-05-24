#' @encoding UTF-8
#' @title Detect Outliers
#' @description Perform exploratory test to detect \emph{outliers}.
#'
#' @param x A numeric object, a vector.
#' @param index A numeric value to be considered in the computations.
#' @param \dots Parameters which are typically ignored.
#'
#' @return Returns the minimum and maximum values, respectively preceded by their positions in the \code{vector}, \code{matrix} or \code{data.frame}.
#' @details The quantity in \emph{min} reveals the minimum deviation from the mean, the integer value in the \emph{closest} indicates the position of that element. The quantity in \emph{max} is the maximum deviation from the mean, and the \code{farthest} integer value indicates the position of that value.
#'
#' @references Dixon, W.J. (1950) Analysis of extreme values. \emph{Ann. Math. Stat.} \bold{21(4),} 488--506.
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#'
#' @seealso \link{Winsorize} for reduce the impact of outliers.
#' @keywords Exploratory
#' @rdname Outlier
#' @export
#' @examples
#' Outlier(x <- rnorm(20))
#'
#' #data frame:
#' age <- sample(1:100, 1000, rep=TRUE);
#' Outlier(age)
#'
`Outlier` <- function(x, index = NULL, ...) UseMethod("Outlier")
#'
#' @rdname Outlier
#' @export
`Outlier.default` <- function(x, index = NULL, ...) {
  if (is.data.frame(x)) {
    as.data.frame(sapply(x, Outlier, index))
  } else if (is.matrix(x)) {
    apply(x, 2, Outlier, index)
  } else if (is.list(x)) {
    lapply(x, Outlier, index)
  } else if (is.vector(x)) {
    if (!is.null(index)) {
      if (!is.list(index)) {
        index <- list(index) # make sure index is a list
      }
      unsplit(Outlier(split(x, index), index = NULL), index)
    } else {
      mu <- Mean(x)
      dev <- abs(x - mu)
      closest <- which.min(dev)
      farthest <- which.max(dev)
      min <- dev[closest]
      max <- dev[farthest]
      output <- data.frame(closest, min, farthest, max)
      class(output) <- c("SciencesPo", class(output))
      attr(output, "scpo.type") <- "Standard"
      return(output)
    }
  } else {
    cat("Non-numeric argument to 'outlier'", class(x), "\n", sep = "")
  }
}
NULL
