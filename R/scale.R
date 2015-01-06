#' @encoding UTF-8
#' @title Scalling Method
#' 
#' @param x data vector or variable to be scaled.
#' @param \dots optional arguments that are passed to scale.
#'  
#' @export
scale <- function(x, ...) 
  UseMethod("scale")
NULL


#' @encoding UTF-8
#' @title  Standardized Beta Coefficients 
#' 
#' @description Calculates standardized beta coefficients from \code{lm} or \code{glm} class model objects. Standardized coefficients refer to how many standard deviations a dependent variable will change per standard deviation increase in the predictor variable. See details.
#' @param x a model object.
#' @param std.dev an integer value for the standard deviation, default is \code{std.dev = 1}.
#' 
#' @return Standardized coefficients
#' 
#' @details Standardized beta coefficients are values estimated from unstandardized coefficients, which are only partially adjusted by the quotient of the standard deviation of the independent variable (IV), and the standard deviation of the dependent variable. Essentially, \bold{beta coefficients report the relative importance of each independent variables}. Therefore, standardized coefficients are rather important for multiple regression models, once they may aid on identifying critical IVs.
#' 
#' Baguley, T. (2009) Standardized or simple effect size: What should be reported?. \emph{British Journal of Psychology,} \bold{100(3),} 603-617.
#' Gelman, A. (2008) Scaling regression inputs by dividing by two standard deviations. \emph{Statistics in Medicine}, \bold{27:} 2865–2873.
#' Gelman, A., and Hill, J. (2006) \emph{Data analysis using regression and multilevel/hierarchical models.} Cambridge University Press.
#' Kleinman, Ken and Horton, Nicholas (2014). \emph{SAS and R: Data Management, Statistical Analysis, and Graphics}.Chapman and Hall/CRC.
#' 
#' @keywords Data-Analysis 
#' @keywords Models
#' @keywords Standardization 
#'
#' @examples
#' df <- read.csv("http://www.math.smith.edu/r/data/help.csv") #get some data
#' female <- subset(df, female==1)
#'
#' # fit a linear regression (OLS) model.
#' (model <- lm(pcs ~ mcs + homeless, data=female) )
#' 
#' # finally, apply standardization to the coefficients.
#' scale(model)
#' 
#' # Possible interpretation: a change in 1 standard deviation  of mcs has more than 
#' # twice the impact on pcs than a 1 standard deviation change in the homeless variable.
#' # Despite  standardized coefficients improve transparency about the size of the effects, 
#' # this example points to a potential weakness of standardized regression coefficients 
#' # in that the homeless variable can take on values of either 0 or 1, and a 1 standard deviation 
#' # change is hard to interpret in such contexts.
#' # Andrew Gelman makes a compelling argument for standardizing variables by 2 standard deviations
#' # so that the variance is similar to a binary variable (provided p is not too far from 0.5).
#' 
#' @export
scale.lm  <- function(x, std.dev=1)
{
    if (!inherits(x, c("lm","glm") )) stop("object not of class \"lm\"")
    
      coef <- summary(x)$coef[-1, 1]
      sd.x <- sapply(x$model[-1], sd)
      sd.y <- sapply(x$model[1], sd)
      if(std.dev==1){
        #calculate z-scores
        beta <- coef*(sd.x/sd.y)
        cat("Standardized = ", round(beta, 4), ".\n", sep = "")
        return(beta)
      }else{
        beta <- coef*((stdev*sd.x)/(stdev*sd.y))
    cat("Standardized = ", round(beta, 4), ".\n", sep = "")
    return(beta)
      }
    }
NULL


#' @encoding UTF-8
#' @title Unscale Data
#' 
#' @description Unscale objects by dropping \code{scaled:scale} and \code{scaled:center} attributes from parameters.
#' 
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @param x a scaled object 
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