#' Scale data to an arbitrary range.
#'
#' Scale data to a new range. Arguments include the data, the new minimum, and the new maximum values.
#'
#' @param x The data vector.
#' @param new.min An integer for the minimun value for the range.
#' @param new.max An integer for the maximun value for the range.
#' x <- sample(10)
#' new.range(x, 0, 1)
#' @export
new.range <- function(x, new.min=0, new.max=1){
	ranx <- range(x, na.rm=TRUE)
	px <- (x-ranx[1])/(ranx[2]-ranx[1])
	res <- new.min+((new.max-new.min)*px)
	res
}
