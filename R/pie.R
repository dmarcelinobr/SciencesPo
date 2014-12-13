#' Creates a pie chart using ggplot2.
#'
#' @details For a contrary point-of-view, see Spence's article, No Humble Pie: The Origins and
#' Usage of a Statistical Chart (http://www.psych.utoronto.ca/users/spence/Spence%202005.pdf).
#' 
#' @param df the data frame.
#' @param col the name of the column to generate the pie chart for.
#' @param label the label for the legend.
#' @export
#' @examples 
#' data(mtcars)
#' pie(mtcars, )
pie <- function(df, col, label=col) {
	df$pie = df[,col]
	l = levels(df$pie)
	t = table(df$pie)
	levels(df$pie) = paste(names(t), " ", format(100*t/sum(t),digits=1), "%", sep="")
	p = ggplot(df, aes(x=factor(1), fill=pie)) + 
		geom_bar(width=1) + coord_polar(theta="y") + xlab("") + ylab("") + 
		scale_fill_hue(name=label, breaks=levels(df$pie), labels=levels(df$pie)) + 
		 theme_minimalist() + theme(axis.text.y=element_blank(), axis.text.x=element_blank(), axis.ticks=element_blank()) 
	print(p)
}