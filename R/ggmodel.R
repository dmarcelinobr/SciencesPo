ggmodel <- function(model, toplot="points", which=c(1:3, 5), mfrow=c(1,1), 
											what = "plots", ...)
{
	.fitted <- NULL
	.resid <- NULL
	.responsevar <- NULL
	.cooksd <- NULL
	.hat <- NULL
	.stdresid <- NULL
	.rows <- NULL
	
	df <- fortify(model)
	df <- cbind(df, .rows=1:nrow(df))
	
	if(!what == "plots"){
		return(df)
	} else
	{
	if(toplot == "points"){
		toplot <- geom_point()
	} else if(toplot == "labels")
		{ toplot <- geom_text(aes(label=.rows)) }
	
	# residuals vs fitted
	g1 <- ggplot(df, aes(.fitted, .resid)) +
		toplot +
		geom_smooth(se=FALSE, method="loess") +
		geom_hline(linetype=2, size=.2, aes(yintercept=0)) +
		scale_x_continuous("Fitted Values") +
		scale_y_continuous("Residual") +
		ggtitle("Residuals vs Fitted")
	
	# normal qq	
# 	a <- quantile(df$.stdresid, c(0.25, 0.75))
# 	b <- qnorm(c(0.25, 0.75))
# 	slope <- diff(a)/diff(b)
# 	int <- a[1] - slope * b[1]
	g2 <- ggplot(df, aes(sample=.resid)) +
		stat_qq() +
# 		geom_abline(slope=slope, intercept=int) +
		scale_x_continuous("Theoretical Quantiles") +
		scale_y_continuous("Standardized Residuals") +
		ggtitle("Normal Q-Q")
	
	# histogram of response variable
  df2 <- df
  names(df2)[1] <- ".responsevar"
	g3 <- ggplot(df2, aes(x=.responsevar)) +
	  geom_histogram() +
	  scale_y_continuous("Response variable") +
	  ggtitle("Response Variable Histogram")

  # histogram of residuals
	g4 <- 
  ggplot(df, aes(.resid)) +
		geom_histogram() +
		scale_y_continuous("Residuals") +
		ggtitle("Residuals Histogram")
	
	# cook's distance
	horizline <- 4/length(model$residuals)
	g5 <- ggplot(df, aes(.rows, .cooksd, ymin=0, ymax=.cooksd)) +
		toplot + 
		geom_linerange() +
	  geom_hline(y=horizline, linetype=2) + 
		scale_x_continuous("Observation Number") +
		scale_y_continuous("Cook's distance") +
		ggtitle("Cook's Distance")
	
	# residuals vs leverage
	g6 <- ggplot(df, aes(.hat, .stdresid)) +
		toplot +
		geom_smooth(se=FALSE, method="loess") +
		geom_hline(linetype=2, size=.2) +
		scale_x_continuous("Leverage") +
		scale_y_continuous("Standardized Residuals") +
		ggtitle("Residuals vs Leverage")
		
	# cooksd vs leverage
# 	g6 <- ggplot(df, aes(.hat, .cooksd)) +
# 		toplot +
# 		geom_smooth(se=FALSE, method="loess") +
# 		scale_x_continuous("Leverage") +
# 		scale_y_continuous("Cook's distance") +
# 		ggtitle("Cook's dist vs Leverage")
# 	
	plots <- list(g1, g2, g3, g4, g5, g6)
	
	# making the plots
	grid.newpage()
	
	if (prod(mfrow)>1) {
		mypos <- expand.grid(1:mfrow[1], 1:mfrow[2])
		mypos <- mypos[with(mypos, order(Var1)), ]
		pushViewport(viewport(layout = grid.layout(mfrow[1], mfrow[2])))
		formatter <- function(.){}
	} else {
		mypos <- data.frame(matrix(1, length(which), 2))
		pushViewport(viewport(layout = grid.layout(1, 1)))
		formatter <- function(.) {
		.dontcare <- readline("Hit <Return> to see next plot: ")
			grid.newpage()
		}
	}
	
	j <- 1
	for (i in which){
		formatter()
		print(plots[[i]], vp=viewport(layout.pos.row=mypos[j,][1], layout.pos.col=mypos[j,][2]))
		j <- j+1
	}
	}
}