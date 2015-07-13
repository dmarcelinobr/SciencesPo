#' Check for differences in data.frames
#'
#' @param A The original data object.
#' @param B The other data.frame
#' @export
setdiff.data.frame = function(A, B){
  g <-  function(y, B){
    any( apply(B, 1, FUN = function(x)
      identical(all.equal(x, y), TRUE) ) ) }
  unique( A[ !apply(A, 1, FUN = function(t) g(t, B) ), ] )
}
NULL