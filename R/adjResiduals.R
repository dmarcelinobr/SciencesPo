#' @encoding UTF-8
#' @title Adjusted Residuals
#' @description Calculates adjusted residuals from a linear fitted model.
#' @param object a model object of type \code{glm} or \code{lm}.
#' @param \dots further arguments passed to or used by other methods.
#' @author Daniel Marcelino \email{dmarcelino@@live.com}.
#' @importFrom stats residuals lm.influence
#'@export
`adjResiduals` <- function(object, ...) {
  residuals(object, ...) / sqrt(1 - lm.influence(object)$hat)
}
NULL
