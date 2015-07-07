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
#' x <- c(1:5)
#' y <- c(1.1,1.5,2.9,3.8,5.2)
#' sd <- (0.2,0.3,0.2,0.0,0.4)
#' @export
`errorbars` <- function(x, y, ebl, ebu = ebl, length = 0.08, ...){
  arrows(x, y + ebu, x, y - ebl, angle = 90, code = 3, length= length, ...)
}
NULL


#' @encoding UTF-8
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
#' add.footnote(size = .9, color = "red")
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
`add.footnote` <-
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
#' multiplot(plot1, plot2, ncols=2)
#' @export
`multiplot` <- function(..., ncols=1, layout=NULL) {
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



#' @encoding UTF-8
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
`fade`<- function (color, alpha = .5)
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



#' @encoding UTF-8
#' @title Shades Normal Distribuion
#'
#' @description Produces a plot of a normal density distribution with shaded areas.
#'
#' @param below sets a lower endpoint.
#' @param above sets an upper endpoint.
#' @param pcts the
#' @param mu the mean.
#' @param sigma standard deviations.
#' @param numpts the number os points/observations to drawn upon.
#' @param color the color of the area.
#' @param dens the density of the color.
#' @param justabove just plots the upper tail.
#' @param justbelow just plots the lower tail.
#' @param lines to draw lines.
#' @param between plots between specified points.
#' @param outside alternative "outside" area.
#'
#' @return A plot with a normal distribution density with shaded areas
#'
#'@examples
#' shadenorm()
#' shadenorm(below=-1.5)
#' shadenorm(below=-1.5,justbelow=TRUE)
#' shadenorm(above=1.5, justabove=TRUE)
#' shadenorm(below=-1.5,above=1.5)
#' shadenorm(between=c(-4,0),color="black")
#' shadenorm(between=c(0,4),color="black")
#' shadenorm(between=c(-1,+1),color="darkgray")
#' title("P[-1 < z < 1] = 68%")
#' shadenorm(between=c(-2,+2),color="darkgray")
#' title("P[-2 < z < 2] = 95%")
#' shadenorm(between=c(-3,+3),color="darkgray")
#' title("P[-3 < z < 3] = 99.7%")
#' shadenorm(between = c(-1.75, 0, 2, 0.5, -1))  ## Plots between specified points
#' shadenorm(below=50,justbelow=TRUE,color="black",mu=47.3,sigma=9.3)
#'
#' ## Can plot one and then another on top of it using lines = TRUE
#' shadenorm(mu=2, sigma=10, outside=c(-3, 12), dens=15)
#' shadenorm(mu=2, sigma=15, between=c(-3, 12),lines=TRUE, col="blue",dens=15)
#' ## Example: Plotting a Hypothesis Test for the mean
#' ## Truth:      mu.true  = 8
#' ## Hypothesis: mu.ho    = 6
#' ## Generate Data Under Truth
#' mu.true = 5 ## Alternative Mean
#' mu.ho   = 6
#' sig     = 8
#' N       = 250 ## Sample Size
#'
#' std.err = sig/sqrt(N)
#' crits = qnorm(c(0.025,0.975),mean=mu.ho, sd = std.err)
#' shadenorm(outside = crits, mu = mu.ho, sigma = std.err,dens=15)
#' shadenorm(between = crits, mu = mu.true, sigma = std.err, lines=TRUE, color="green",dens=15)
#'
#'@export
`shadenorm` <- function(below=NULL, above=NULL, pcts = c(0.025,0.975), mu=0,
sigma=1, numpts = 500, color = "gray", dens = 40, justabove= FALSE, justbelow = FALSE, lines=FALSE, between=NULL, outside=NULL) {

  if(is.null(between)){
    below = ifelse(is.null(below), qnorm(pcts[1],mu,sigma), below)
    above = ifelse(is.null(above), qnorm(pcts[2],mu,sigma), above)
  }

  if(is.null(outside)==FALSE){
    below = min(outside)
    above = max(outside)
  }
  lowlim = mu - 4*sigma
  uplim  = mu + 4*sigma

  x.grid = seq(lowlim,uplim, length= numpts)
  dens.all = dnorm(x.grid,mean=mu, sd = sigma)
  if(lines==FALSE){
    plot(x.grid, dens.all, type="l", xlab="X", ylab="Density")
  }
  if(lines==TRUE){
    lines(x.grid,dens.all)
  }

  if(justabove==FALSE){
    x.below    = x.grid[x.grid<below]
    dens.below = dens.all[x.grid<below]
    polygon(c(x.below,rev(x.below)),c(rep(0,length(x.below)),rev(dens.below)),col=color,density=dens)
  }
  if(justbelow==FALSE){
    x.above    = x.grid[x.grid>above]
    dens.above = dens.all[x.grid>above]
    polygon(c(x.above,rev(x.above)),c(rep(0,length(x.above)),rev(dens.above)),col=color,density=dens)
  }

  if(is.null(between)==FALSE){
    from = min(between)
    to   = max(between)

    x.between    = x.grid[x.grid>from&x.grid<to]
    dens.between = dens.all[x.grid>from&x.grid<to]
    polygon(c(x.between,rev(x.between)),c(rep(0,length(x.between)),rev(dens.between)),col=color,density=dens)
  }
}
NULL






