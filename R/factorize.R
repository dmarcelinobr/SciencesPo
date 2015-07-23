#' @encoding UTF-8
#' @title Conditionally convert vectors to factors
#'
#' @description A generic function and several instances for creating factors from
#' other sorts of data. The primary use case is for vectors that contain
#' few unique values and might be better considered as factors. When
#' applied to a data frame, this is applied to each column in the data.frame.
#'
#' @param x an object.
#' @param max.levels an integer that determines the number of unique values to be coverted. Default is \code{max.levels = 10}.
#' @param ... additional arguments (currently ignored)
#'
#' @examples
#' #Some data
#' ID = 1:10
#' Age = round(rnorm(10,50,1))
#' diag = c("Depression","Bipolar");
#' Diagnosis = sample(diag, 10, replace=TRUE)
#' data = data.frame(ID, Age, Diagnosis)
#' factorize(data$Diagnosis)
#' str(factorize(data))
#' @export
`factorize` <- function(x,  ...) {
  UseMethod("factorize")
}

#' @rdname factorize
#' @export
`factorize.default` <- function(x, ...) {
  x
}

#' @rdname factorize
#' @export
`factorize.numeric` <- function(x, max.levels = 10L, ...){
  if (length(unique(x)) <=  max.levels) return ( factor(x, levels=sort(unique(x))) )
  x
}

#' @rdname factorize
#' @export
`factorize.character` <- function(x, max.levels = 10L, ...){
  if (length(unique(x)) <=  max.levels) return ( factor(x, levels=sort(unique(x))) )
  x
}

#' @rdname factorize
#' @export
`factorize.data.frame` <- function(x, max.levels=10L, ...) {
  as.data.frame( lapply(x, factorize, max.levels=max.levels) )
}
NULL
