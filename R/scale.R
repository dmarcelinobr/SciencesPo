#'


@export
scale <- function(x, ...) UseMethod("scale")

#'
#'
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
@export
scale.lm  <- function(x, std=1)
{
    if (!inherits(x, c("lm","glm") )) stop("object not of class \"lm\"")
    
      coef <- summary(mod)$coef[-1, 1]
      sd.x <- sapply(mod$model[-1], sd)
      sd.y <- sapply(mod$model[1], sd)
      if(std==1){
        #calculate z-scores
        beta <- coef*(sd.x/sd.y)
        cat("Standardized = ", round(beta, 4), ".\n", sep = "")
        return(beta)
      }else{
        beta <- coef*((std*sd.x)/(std*sd.y))
    cat("Standardized = ", round(beta, 4), ".\n", sep = "")
    return(beta)
      }
    }
NULL

#' @title Unscale 
#' 
#' @description Unscale objects by dropping \code{scaled:scale} and \code{scaled:center} attributes from parameters.
#' 
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @param x A scaled object 
#' @return Unscaled parameters 
#' @keywords Models
#' @export
unscale <- function(x)
{
  if(!is.null(attr(x,"scaled:scale")))
    x <- sweep(x, FUN="*", 2, attr(x,"scaled:scale"))
  if(!is.null(attr(x,"scaled:scale")))
    x <- sweep(x, FUN="+", 2, attr(x,"scaled:center"))
  attr(x,"scaled:scale") <- NULL
  attr(x,"scaled:center") <- NULL
  x
}