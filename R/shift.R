#' @encoding UTF-8
#' @title Shifts elements of a vector left or right by N positions (Lag or Lead).
#'
#'@aliases shift shift.default shift.data.frame
#'@param x A vector to be operated on
#'@param n Number of rows to shift by (if negative, shift to right instead of
#'left)
#'@param wrap Whether to wrap elements or not (adds the entry at the beginning to the end)
#'@param pad Whether to pad with NAs or not.  pad does nothing unless wrap is
#'false, in which case it specifies whether to pad with NAs
#'@param \dots Other items to pass along
#'@return vector of the same type as vec
#'@examples
#' l <- list(a = sample(LETTERS,3), b = runif(5), c = runif(15));
#' df <- as.data.frame(l);
#'shift(df$c, 1)
#'
#' @export shift
#'@rdname shift
`shift` <- function(x,...) {
  UseMethod("shift",x)
}
#' @method shift default
#' @export
#' @rdname shift
shift.default <- function(x,n=1,pad=TRUE, wrap=TRUE,...) {
  if(length(x)<abs(n)) {
    #stop("Length of vector must be greater than the magnitude of n \n")
  }
  if(n==0) {
    return(x)
  } else if(length(x)==n) {
    # return empty
    length(x) <- 0
    return(x)
  } else if(n>0) {
    returnvec <- x[seq(n+1,length(x) )]
    if(wrap) {
      returnvec <- c(returnvec,x[seq(n)])
    } else if(pad) {
      returnvec <- c(returnvec,rep(NA,n))
    }
  } else if(n<0) {
    returnvec <- x[seq(1,length(x)-abs(n))]
    if(wrap) {
      returnvec <- c( x[seq(length(x)-abs(n)+1,length(x))], returnvec )
    } else if(pad) {
      returnvec <- c( rep(NA,abs(n)), returnvec )
    }

  }
  return(returnvec)
}
#' @method shift data.frame
#' @export
#' @rdname shift
#' @importFrom plyr colwise
`shift.data.frame` <- function(x,...) {
  colwiseShift <- colwise(shift.default)
  colwiseShift(x,...)
}

