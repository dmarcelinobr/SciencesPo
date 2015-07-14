
#' @encoding UTF-8
#'@title Calculates the true median
#' @description Usually median for data with ties, the tied values are treated as exactly the same. For instance, taking a median of 3, 3, 4, 4, 4 will be 4. However, the values to be measured are usually rounded off, so that we can assume evenly distributed true values for tied values. For instance, the previous data can be treated as rounded values of 2.75, 3.25, 11/3, 4, 13/3. From this viewpoint, the true median of 3, 3, 4, 4, 4 could be 11/3 (=3.66...).
#' @param x a numeric vector.
#' @param h	width of measurement unit. Default is \code{h=1}.
#' @author Daniel Marcelino \email{dmarcelino@@live.com}
#'@examples
#' s <-c(3, 3, 4, 4, 4)
#' trueMedian(s)
#' median(s)
#' z <- c(2.75, 3.25, 11/3, 4, 13/3)
#' trueMedian(z)
#' median(z)
#' @export
#'
#' @importFrom stats median
#'
`trueMedian` <- function(x,h=1) {
  YY <- rep(0,length(x))
  XX <- table(x)
  q <- length(XX)
  k <- 0
  for (i in 1:q) {
    L <- as.numeric(names(XX)[i])-h/2
    for (j in 1:XX[[i]]) {
      k <- k+1
      YY[k] <- L+h*(2*j-1)/(2*XX[[i]])
    }
  }
  median(YY)
}
NULL
