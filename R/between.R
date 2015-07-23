#' @title  Classify values into groups
#'
#' @description  Classify values into groups based on which numbers they're between.
#'
#'@param x Numeric vector to classify
#'@param chunks Vector listing what values the grouping should be done on. Should include the max and the min in this list as well.
#'@return Vector of length(x) indicating which group each element is in (for between). Or vector of length(x) indicating the lower bound of the group that it is in.
#'@seealso \code{\link{dummy}}.
#'@export between
#'@examples
#' age <-sample(1:100, 1000, rep=TRUE)
#' between(age,c(0, 15, 30, 50, 100))
#'
#' @rdname bin
between = function(x,chunks) {
  n <- length(chunks) - 1
  chunks_hi <- chunks[seq(2,length(chunks))]
  chunks_lo <- chunks[seq(length(chunks)-1)]
  tweened <- rep(NA,length(x))
  for(i in seq(n)) {
    tweened[x >= chunks_lo[i] & x < chunks_hi[i]] <- i
  }
  tweened[x >= chunks_hi[n]] <- n

  return(tweened)
}
NULL
