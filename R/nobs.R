#' @title Number of observations
#'
#' @param object the data object
#' @param \dots some extra parameters.
#' @return The number of observations.
#' @docType methods
#' @rdname nobs-methods
#'
#' @examples
#' nobs(1:50)
#' nobs(10)
#' nobs(as.matrix(1:10))
#'
#' @export
setGeneric("nobs", function(object, ...){
  standardGeneric("nobs")
})
NULL


#' @rdname nobs-methods
#' @aliases nobs,numeric,ANY-method
#' @export
setMethod("nobs", "numeric", function(object, ...){
  length(object)
})
NULL


#' @rdname nobs-methods
#' @aliases nobs,integer,ANY-method
#' @export
setMethod("nobs", "integer", function(object, ...){
  length(object, ...)
})
NULL


#' @rdname nobs-methods
#' @aliases nobs,matrix,ANY-method
#' @export
setMethod("nobs", "matrix", function(object, ...){
  NROW(object, ...)
})
NULL



#' @rdname nobs-methods
#' @aliases nobs,data.frame,ANY-method
#' @export
setMethod("nobs", "data.frame", function(object, ...){
  NROW(object)
})
