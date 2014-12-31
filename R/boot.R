#' @title Generic Method for Bootstrap
#'
#' This method is intended to be overried by statistical models that would like
#' to support statistical bootstrapping.
#' @note This method has private memory storage and can reference the objects:
#' \dQuote{.fitted}, \dQuote{.data}, \dQuote{.call}, \dQuote{.env}, despite having no declaration in
#' the argument list.
#' @param mod a fitted model object that will be used to produce boot-strapped
#' parameters. This object usually inherits the class \dQuote{glm} or \dQuote{lm} object
#' @param ... unspecified parameters
#' @return a list with the \dQuote{alpha} and \dQuote{beta} slots set. Note that \dQuote{alpha}
#' corresponds to ancillary parameters and \dQuote{beta} corresponds to systematic
#' components of the model
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @export
boot <- function (mod, ...)
  UseMethod("boot")

#' @title Bootstrap Parameters of Statistical Model
#'
#' This method is the default for bootstrapping statistical models
#' 
#' @param mod A fitted model object, typically of class \dQuote{lm} or \dQuote{glm}
#' @param ... A list of optional parameters
#' @return a list with the \dQuote{alpha} and \dQuote{beta} slots set
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @export
boot.default <- function (mod, ...)
  list(
       alpha = NULL,
       beta = coef(mod)
       )
