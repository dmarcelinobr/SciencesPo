#' @title Center Predictor Variables.
#'
#' @description  Perform column centering and allow for centering by groups. By centering, I simply mean to subtract a constant from every value of a variable (x - \bar{x}), which produces an equivalent as \code{scale(x, scale = FALSE)} in the \pkg{base} package.
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
#' require(ggplot2)
#' #have random normal distributions with means of 2 and 5, respectively.
#' n <- 20
#' df = data.frame(id=1:n, x=rnorm(n, mean=2, sd=.5), y=rnorm(n, mean=5, sd=2), 
#' age=rnorm(n, mean = 40, sd = 30), female=sample(c(TRUE, FALSE), n, rep = TRUE) )
#' mod = lm(y ~ x, data=df)
#' summary(mod)
#' ggplot(df, aes(x=x, y=y)) + geom_point()
#' ggplot(df, aes(x=x, y=y)) + geom_point() + geom_abline(intercept=mod$coefficients[1], slope=mod$coefficients[2]) + stat_smooth()
#' 
#' 
#' # centering
#' mod = lm(center(y) ~ center(x), data=df)
#' summary(mod)
#'  # center and z-score: (x - xbar)/sd(x)
#'  # mod = lm(rescale(center(y)) ~ rescale(center(x)), data=df)
#'  # as.beta(mod) # after fitting
#' 
#'  with(df, center(y, as.factor(female))) # center by group  
#'
#' @keywords Standardization
#'
#' @seealso \code{\link{normalize}}
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
