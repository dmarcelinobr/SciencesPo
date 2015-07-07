#' @encoding UTF-8
#' @title Multiple ggplot objects
#' @description ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
#' @param \dots a list of ggplot objects
#' @param ncols number of columns in layout
#' @param layout a matrix specifying the layout. If present, 'ncols' is ignored.
#' @details If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE), then plot 1 will go in the upper left, 2 will go in the upper right, and 3 will go all the way across the bottom.
#' @details Function based on the Winston Chang's R cookbook
#' @importFrom grid pushViewport
#' @importFrom grid viewport
#' @importFrom grid grid.newpage
#' @importFrom grid grid.layout
#' @examples
#'  # Load the diamonds dataset
#' data(diamonds)
#' # Create a histogram, assign to "plot1"
#' plot1 <- qplot(price,data=diamonds,binwidth=1000)
#'  # Create a scatterplot
#' plot2 <- qplot(carat,price,data=diamonds)
#' # Arrange and display the plots into a 1x2 grid
#' ggMultiplot(plot1, plot2, ncols=2)
#' @export
`ggMultiplot` <- function(..., ncols=1, layout=NULL) {
  .plotlist=NULL
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), .plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, ncols * ceiling(numPlots/ncols)),
                     ncol = ncols, nrow = ceiling(numPlots/ncols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
NULL
