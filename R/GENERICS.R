#' @title Method for adding things
#'
#' @param \dots some extra parameters.
#' @export
#' @docType methods
#' @rdname add-methods
#'
setGeneric("add", function(...){
  standardGeneric("add")
})



#' @title Method for building things
#'
#' @param \dots some extra parameters.
#' @export
#' @docType methods
#' @rdname build-methods
#'
setGeneric("build", function(...){
  standardGeneric("build")
})


#' Mehtod of comparing things
#'
#' @description
#' Generic function for comparing things.
#'
#' @param object a data object.
#' @param \dots Ignored parameters.
#' other parameters
#'
`compare`<-function(object,...){
  UseMethod("compare")
}


#' Is an unique identifier
#'
#' @description
#' Generic function for checking for a unique row identifier.
#'
#' @param object a data object.
#' @param \dots Ignored parameters.
#'
`isid`<-function(object,...){
  UseMethod("isid")
}



#' Mehtod of Kalman Filtering
#'
#' @param object a data object.
#' @param \dots Ignored parameters.
#'
`kalman.filter` <-function(object,...){
  UseMethod("kalman.filter")
}


#' Mehtod for experiment
#'
#' @param object a data object.
#' @param \dots Ignored parameters.
#'
`experiment` <-function(object, ... )
  UseMethod("experiment")


#' Mehtod for project
#'
#'
#' @param object a data object.
#' @param \dots Ignored parameters.
#'
`project` <-function(object, ... )
  UseMethod("project")
