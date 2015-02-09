#' @encoding UTF-8
#' @title Add Error Bars on the Graph
#' 
#' @description Draw error bars on the graph.
#' @param x coordinates for the error bars, or simply the name of the graph.
#' @param y coordinates for the center of the error bars, which is the group means.
#' @param ebl length of the error bars, which should be 1 se in each direction.
#' @param ebu the error bars.
#' @param length the length. 
#' @param \dots typically additional noninteresting arguments to pass.
#' 
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' 
#' @export
addErrorBar <- function(x, y, ebl, ebu = ebl, length = 0.08, ...){
  arrows(x, y + ebu, x, y - ebl, angle = 90, code = 3, length= length, ...)
}
NULL


#' @title Add Footnote to a ggplot Object
#'
#' @description Add footnotes to \pkg{ggplot2} objects.
#'
#' @param x any text or empty to use default.
#' @param size the font size \code{x}.
#' @param rotation the rotation for the footnote, default is \code{rotation=90}.
#' @param color the color for \code{x}.
#' @param justification the justification method.
#'
#' @details At this stage, this function only works for a ggplot object.
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#'
#' @examples
#' # setup data
#' x <- seq(0, 50, 1)
#' supply <- x * -2 + 100
#' demand <- x * 2
#' df <- data.frame( x = x, supply=supply, demand=demand)
#'
#' library(ggplot2)
#' ggplot(df, aes(x)) +
#' geom_line(aes(y = supply, colour = supply))+
#' geom_line(aes(y = demand, colour = demand))
#' addFootnote(size = .9, color = "red")
#'
#' @keywords Graphs
#'
#' @importFrom ggplot2 ggplot
#' @importFrom grid unit
#' @importFrom grid pushViewport
#' @importFrom grid popViewport
#' @importFrom grid grid.text
#' @importFrom grid gpar
#' @importFrom grid viewport
#'
#' @export
#'
addFootnote <-
  function(x=NULL, size=NULL, color=NULL, rotation = 90, justification = c("right", "bottom")) {
    if(!is.null(x)){
      text = paste(x)
    } else{ 
      text = paste(Sys.info()["user"],
                   format(Sys.time(), "%d %b %Y"),sep = " " ) 
    }
    if(is.null(size)){
      size = .65
    } 
    if(is.null(color)){
      color = grey(.75)
    }
    pushViewport(viewport())
    grid.text(label = text ,
              x = unit(1,"npc") - unit(2, "mm"),
              y = unit(35, "mm"),
              just = justification,
              rot = rotation,
              gp = gpar(cex = size, col = color))
    popViewport()
  }
NULL




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
#' multiPlot(plot1, plot2, ncols=2)
#' @export
multiPlot <- function(..., ncols=1, layout=NULL) {
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




#' @title Add transparency
#'
#' @description Alpha function to add transparency in graphic objects
#'
#' @param color Any color or vector of colors
#' @param alpha Level for alpha, default is \code{0.5}
#' 
#' @keywords Graphs
#' 
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' 
#' @examples
#' # setup data
#' x <- seq(0, 50, 1)
#' supply <- x * -2 + 100
#' demand <- x * 2
#' # Point size and transparency 
#' plot(supply, demand, pch = 19, cex = 3, col = fade("red", 0.5))
#'
#'@export
#'
fade<- function (color, alpha = .5) 
{
  if(missing(color))
    stop("vector of colors missing")
  col <- col2rgb(color, TRUE)/255
  if (length(color) != length(alpha)) {
    if (length(color) > 1 && length(alpha) > 1) {
      stop("Only one color and alpha can be vectorised!")
    }
    if (length(color) > 1) {
      alpha <- rep(alpha, length.out = length(color))
    }
    else if (length(alpha) > 1) {
      col <- col[, rep(1, length(alpha)), drop = FALSE]
    }
  }
  alpha[is.na(alpha)] <- col[4, ][is.na(alpha)]
  alpha_col <- rgb(col[1, ], col[2, ], col[3, ], alpha)
  alpha_col[is.na(color)] <- NA
  alpha_col
}
NULL

