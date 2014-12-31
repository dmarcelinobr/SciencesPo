#' @title Center Predictor Variables.
#' 
#' @description  Perform column centering also by groups. Centering simply means subtracting a constant from every value of a variable.
#' 
#' @param x A variable name whose values is to be centered 
#' @param by A factor variable 
#' 
#' @return Scaled values 
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#'
#' @details Essentially, what it does is redefine the 0 point for that predictor to be whatever value you subtracted.  It shifts the scale over, but retains the units. In a linear model, the effect is that the slope between that predictor and the response variable doesn't change at all. But the interpretation of the intercept does.  When 0 is out of the range of data, the intercept value is \dQuote{meaningless}. But when \code{x} is centered, the mean of the response when all predictors = 0, becomes actually 0; thus the intercept becomes the mean of Y at the value you centered on.
#'
#' @examples
#' data(tobaccovote)
#' with(tobaccovote, center(money))
#' with(tobaccovote, center(money, as.factor(state))) # center by group
#'
#' @keywords Standardization
#'
#' @seealso  \code{\link{rescale}}, \code{\link{standardize}}
#' @export
#'
center <- function(x,by) {
	if(nargs() > 1 && is.numeric(x) && is.factor(by)) {
    return(x-tapply(x, by, mean, na.rm = TRUE)[by])
}
	if(nargs() == 1 && is.numeric(x)) {
    return(x - mean(x, na.rm = TRUE))
	} else {
		stop("`x` must be a numeric object and `by` a factor") 
	}
}
