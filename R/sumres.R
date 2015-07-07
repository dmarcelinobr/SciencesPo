#' @encoding UTF-8
#' @title Sumarizes the residuals from a linear model
#'
#' @description Simply sumarizes the residuals from a linear model.
#'#' @param x The fitted model object.
#' @examples
#' model<- glm(child~parent, data=galton)
#' sumres(model)
#' @export
`sumres` <-
  function(x) {
    sr <- summary(residuals(x))
    srm <- mean(residuals(x))
    if (abs(srm) < 1e-10){
      sr <- sr[c(1:3,5:6)]
    }
    sr
  }
NULL
