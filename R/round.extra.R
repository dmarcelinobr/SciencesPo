#' @encoding UTF-8
#' @title Round a number
#'
#' @description Given a number or numeric vector, rounds each value up or down something nice for plotting.
#'
#' @param x Number or numeric vector to be rounded.
#' @param down If FALSE, values are rounded up instead of down (default).
#'
#' @return Returns a value or numeric vector of rounded valules.
#'
#' @examples
#'
#'   rough <- sort(runif(10, min=-3, max=3))
#'   data.frame(down=roundExtra(rough, down=TRUE),
#'   value=rough, up=roundExtra(rough))
#'
#' @export
`roundExtra` <- function(x, down=TRUE) {
  if(length(x) > 1) {
    X<- sapply(x, function(v) roundExtra(v, down))
    return(X)
  } else {
    if(sign(x) == 0) return(0)
    expOnTen <- floor(log10(abs(x)))
    mantissa <- abs(x)/10^expOnTen
    if(sign(x) < 0) down <- !down
    if(down) {
      if(mantissa < 1.25) mantissa <- 1
      else mantissa <- floor(2 * mantissa) / 2
    } else {
      if(mantissa < 1.25) mantissa <- 1.25
      else mantissa <- ceiling(2 * mantissa) / 2
    }
    return(sign(x) * mantissa * 10^expOnTen)
  }
}
