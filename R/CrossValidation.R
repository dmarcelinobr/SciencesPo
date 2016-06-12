#' @title Cross Validation of SciencesPo Trees and Forests
#' @description Cross Validation of SciencesPo Trees and Forests.
#' @param FUN either \code{tree} or \code{forest}
#' @param object a fitted \code{Tree} or \code{Forest} object in the formula.
#' @param ... additional arguments.
#' @return An object of class \code{CV} with elements: \describe{
#'   \item{\code{call}}{the function call}
#' }
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}.
#' @rdname CrossValidation
#' @export
CrossValidation <- function(object, ...) {
  UseMethod("CrossValidation")
}

#' @rdname CrossValidation
#' @export
CrossValidation.Tree <- function(object, FUN, ...)
{
  results <- invisible()
  class(results) <- "CV"
  return(results)
}

#' @rdname CrossValidation
#' @export
CrossValidation.Forest <- function(object, FUN, ...) {
  results <- invisible()
  class(results) <- "CV"
  return(results)
}
