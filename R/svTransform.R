#' @encoding UTF-8
#' @title Transform dependent variable
#' @description Simple function to transform a dependent variable that in [0,1] rather than (0, 1) to beta regression. Suggested by Smithson & Verkuilen (2006).
#'
#' @param y the dependent variable in [0, 1] interval.
#' @references
#' Smithson M, Verkuilen J (2006) A Better Lemon Squeezer? Maximum-Likelihood Regres- sion with Beta-Distributed Dependent Variables. \emph{Psychological Methods}, 11(1), 54-71.
#'
#' @seealso  \code{\link{normalize}}.
#' @examples
#'  x <- sample(10);
#'  y <- normalize(x, range=c(0,1));
#'  y;
#'  svTransform(y)
#' @export
`svTransform` <- function(y)
{
  n <- length(y)
  trans <- (y * (n-1) + 0.5)/n
  return(trans)
}
NULL
