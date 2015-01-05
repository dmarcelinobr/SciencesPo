#' @title Variance Inflation Factor
#' 
#' @description Extracts Variance Inflation Factor from a model of class \dQuote{lm}
#' 
#' @param model a model object
#' @param \dots Options to pass to
#' 
#' @return A numeric value indicating the variance inflation in the model
#' 
#' #' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' 
#' @keywords Models
#' 
#' @examples
#' data(mtcars)
#' m1 <- lm(mpg ~ qsec + hp, data=mtcars)
#' vif(m1)
#' @export
#' 
vif <-
function(model, ...) {
	if (any(is.na(coef(model)))) 
		stop ("there are aliased coefficients in the model")
	v <- vcov(model)
	assign <- attributes(model.matrix(model))$assign
	if (names(coefficients(model)[1]) == "(Intercept)") {
		v <- v[-1, -1]
		assign <- assign[-1]
	}
	else warning("Vifs may not be sensible without intercept.")
	parameters <- labels(terms(model))
	n.parameters <- length(parameters)
	if (n.parameters < 2) stop("model contains fewer than 2 parameters")
	R <- cov2cor(v)
	detR <- det(R)
	result <- matrix(0, n.parameters, 3)
	rownames(result) <- parameters
	colnames(result) <- c("GVIF", "Df", "GVIF^(1/(2*Df))")
	for (parameters in 1:n.parameters) {
		subs <- which(assign == parameters)
		result[parameters, 1] <- det(as.matrix(R[subs, subs])) *
			det(as.matrix(R[-subs, -subs])) / detR
		result[parameters, 2] <- length(subs)
	}
	if (all(result[, 2] == 1)) result <- result[, 1]
	else result[, 3] <- result[, 1]^(1/(2 * result[, 2]))
		result
}
