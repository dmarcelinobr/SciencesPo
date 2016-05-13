#' Statistical description
#'
#' Provides description of a vector, matrix, data.frame.
#' @param x A data frame, matrix, vector, or formula.
#' @param \dots Additional arguments passed to \code{Describe.default}.
#'
#' @examples
#' \dontrun{
#'  Describe(turnout)
#'  desc <- Describe(turnout)
#'  desc$v1   # print description for just v1
#'  desc[c('v2','v3')]    # print description for two variables.
#'  desc[sort(names(desc))] # print in alphabetic order by column names.
#'
#' # Describing part of a data frame:
#'  with(turnout, Describe(v1 ~ v2*v3 + v4) )
#'  with(turnout, Describe(~ v2 + v3) )
#'  with(turnout, Describe(~ v2 + v3, weights=freqs)) # weighted analysis
#' }
#'
#'
#' @export
`Describe` <- function(x, ...)
  UseMethod("Describe")
