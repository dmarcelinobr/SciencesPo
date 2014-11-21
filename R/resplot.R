resplot <-
function(model)
{
	.resid <- NULL
	y <- quantile(model$resid[!is.na(model$resid)], c(0.25, 0.75))
	x <- qnorm(c(0.25, 0.75))
	slope <- diff(y)/diff(x)
	int <- y[1L] - slope * x[1L]
	obj <- ggplot(model, aes(sample=.resid)) +
		theme_minimalist(base_size=18) +
		geom_point(stat = "qq", size=5) +
		geom_abline(slope = slope, intercept = int, color="blue")	
	return(obj)
}
