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


#' @title Add legends to a plot
#' @description  To add special styles of legends.
#'
#' @param \dots Legend parameters.
#' @export
#' @examples
#' par(mar = c(5, 4, 1.4, 0.2))
#' plot(rnorm(50), rnorm(50), col=c("steelblue", "indianred"), pch=20)
#' add.legend("topright", legend=c("Foo", "Bar"), pch=20,
#' col=c("steelblue", "indianred"),
#' horiz=TRUE, bty='n', cex=0.8)

add.legend <- function(...) {
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0),
              mar=c(0, 0, 0, 0), new=TRUE)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
}
NULL


' ggcorr - Plot a correlation matrix with ggplot2
#'
#' Function for making a correlation plot starting from a data matrix, using ggplot2. The function is directly inspired by Tian Zheng and Yu-Sung Su's arm::corrplot function. Please visit \url{http://github.com/briatte/ggcorr} for the latest development and descriptions about ggcorr.
#'
#' @export
#' @param data a data matrix. Should contain numerical (continuous) data.
#' @param method a character string giving a method for computing covariances in the presence of missing values. This must be (an abbreviation of) one of the strings \code{"everything"}, \code{"all.obs"}, \code{"complete.obs"}, \code{"na.or.complete"}, or \code{"pairwise.complete.obs"}.
#' Defaults to \code{"pairwise"}.
#' @param cor_matrix the named correlation matrix to use for calculations.
#' @param palette if \code{nbreaks} has been set to something, a ColorBrewer
#' palette to be used for correlation coefficients. Defaults to \code{"RdYlGn"}.
#' @param name a character string for the legend that shows quintiles of
#' correlation coefficients. Defaults to nothing.
#' @param geom the geom object to use. Accepts either \code{tile} (the default)
#' or \code{circle}, to plot proportionally scaled circles.
#' @param max_size the maximum size for circles, as passed to \code{scale_size_identity}
#' for proportional scaling. Defaults to \code{6}.
#' @param min_size the maximum size for circles, as passed to \code{scale_size_identity}
#' for proportional scaling. Defaults to \code{2}.
#' @param label whether to add correlation coefficients as two-digit numbers
#' over the plot. Defaults to \code{FALSE}.
#' @param label_alpha whether to make the correlation coefficients transparent
#' as they come close to 0. Defaults to \code{FALSE}.
#' @param label_color color for the correlation coefficients. Defaults to \code{"black"}.
#' @param label_round decimal rounding of the correlation coefficients.
#' Defaults to \code{1}.
#' @param nbreaks number of breaks to apply to categorize the correlation
#' coefficients. Defaults to \code{NULL} (continuous scaling).
#' @param digits number of digits to show in the breaks of the correlation
#' coefficients. Defaults to \code{2}.
#' @param drop use the empirical range of correlation coefficients for their categorization,
#' which is \emph{not} recommended (see 'Details'). Defaults to \code{FALSE}.
#' @param low lower color of the gradient for continuous scaling of the correlation
#' coefficients. Defaults to \code{d73027} (red).
#' @param mid mid color of the gradient for continuous scaling of the correlation
#' coefficients. Defaults to \code{d73027} (yellow).
#' @param high upper color of the gradient for continuous scaling of the correlation
#' coefficients. Defaults to \code{1a9850} (red).
#' @param midpoint the midpoint value for continuous scaling of the correlation
#' coefficients. Defaults to \code{0}.
#' @param limits whether to bound the continuous color scaling of the correlation
#' coefficients between -1 and +1. Defaults to \code{TRUE}.
#' @param ... other arguments supplied to geom_text for the diagonal labels.
#' Arguments pertaining to the title or other items can be achieved through ggplot2 methods.
#' @details The \code{nbreaks} argument tries to break up the correlation
#' coefficients into an ordinal color scale. Recommended values for the numbers
#' of breaks are 3 to 11, as values above 11 are visually difficult to separate
#' and are not supported by diverging ColorBrewer palettes.
#'
#' The breaks will range from -1 to +1, unless drop is set to \code{FALSE}, in
#' which case the empirical range of the correlation coefficients is used. The
#' latter is not recommended, as it creates a disbalance between the colors of
#' negative and positive coefficients.
#' @seealso \code{\link{cor}} and \code{\link[arm]{corrplot}}
#' @author Francois Briatte \email{f.briatte@@gmail.com} with contributions from
#' Amos B. Elberg \email{amos.elberg@@gmail.com} and Barret Schloerke \email{schloerke@gmail.com}
#' @importFrom reshape melt melt.data.frame melt.default
#' @examples
#' # Basketball statistics provided by Nathan Yau at Flowing Data.
#' dt <- read.csv("http://datasets.flowingdata.com/ppg2008.csv")
#'
#' # Default output.
#' ggcorr(dt[, -1])
#'
#' # Labelled output, with coefficient transparency.
#' ggcorr(dt[, -1],
#'        label = TRUE,
#'        label_alpha = TRUE)
#'
#' # Custom options.
#' ggcorr(
#'   dt[, -1],
#'   name = expression(rho),
#'   geom = "circle",
#'   max_size = 10,
#'   min_size = 2,
#'   size = 3,
#'   hjust = 0.75,
#'   nbreaks = 6,
#'   angle = -45,
#'   palette = "PuOr" # colorblind safe, photocopy-able
#' ) + ggplot2::labs(title = "Correlation Matrix")
#'
#' # Supply your own correlation matrix
#' ggcorr(
#'   data = NULL,
#'   cor_matrix = cor(dt[,-1], use = "pairwise")
#' )

ggcorr <- function(
  data,
  method = "pairwise",
  cor_matrix = cor(data, use = method),
  palette = "RdYlGn",
  name = "",
  geom = "tile",
  min_size = 2,
  max_size = 6,
  label = FALSE,
  label_alpha = FALSE,
  label_color = "black",
  label_round = 1,
  nbreaks = NULL,
  digits = 2,
  drop = FALSE,
  low = "#d73027",
  mid = "#ffffbf",
  high = "#1a9850",
  midpoint = 0,
  limits = TRUE,
  ...) {

  M <- cor_matrix

  # protect against spaces in variable names
  colnames(M) = rownames(M) = gsub(" ", "_", colnames(M))

  # correlation coefficients
  D <- round(M, label_round)

  D <- D * lower.tri(D)
  D <- as.data.frame(D)

  rowNames <- names(D)
  D <- data.frame(row = rowNames, D)
  D <- melt(D, id.vars = "row")

  # correlation quantiles
  M <- M * lower.tri(M)
  M <- as.data.frame(M)
  M <- data.frame(row = rowNames, M)
  M <- melt(M, id.vars = "row")
  M$value[M$value == 0] <- NA

  if(!is.null(nbreaks)) {

    s = seq(-1, 1, length.out = nbreaks + 1)

    if(!nbreaks %% 2)
      s = unique(sort(c(s, 0)))

    M$value = droplevels(cut(M$value, breaks = s, include.lowest = TRUE, dig.lab = digits))
    M$value = factor(M$value, levels = unique(cut(s, breaks = s, dig.lab = digits, include.lowest = TRUE)))

  }

  if(is.null(midpoint)) {

    midpoint = median(M$value, na.rm = TRUE)
    message("Color gradient midpoint set at median correlation to ", round(midpoint, 2))

  }

  M$row <- factor(M$row, levels = unique(as.character(M$variable)))

  # for circles
  M$num = as.numeric(M$value)
  M$num = abs(M$num - median(unique(M$num), na.rm = TRUE))
  M$num = as.numeric(factor(M$num))
  M$num = seq(min_size, max_size, length.out = length(na.omit(unique(M$num))))[ M$num ]

  diag  <- subset(M, row == variable)
  M <- M[complete.cases(M), ]

  # clean plot panel
  po.nopanel <- list(theme(
    panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.key = element_blank(),
    axis.text.x = element_text(angle = -90))
  )

  p = ggplot(M, aes(x = row, y = variable))

  # apply main geom
  if(geom == "circle") {

    p = p +
      geom_point(aes(size = num + 0.25), color = "grey50") +
      geom_point(aes(size = num, color = value))

    if(is.null(nbreaks) & limits)
      p = p +
        scale_size_continuous(range = c(min_size, max_size)) +
        scale_color_gradient2(name, low = low, mid = mid, high = high, midpoint = midpoint,
                              limits = c(-1, 1)) +
        guides(size = FALSE)
    else if(is.null(nbreaks))
      p = p +
        scale_size_continuous(range = c(min_size, max_size)) +
        scale_fill_gradient2(name, low = low, mid = mid, high = high, midpoint = midpoint) +
        guide(size = FALSE)
    else
      p = p +
        scale_size_identity(name) +
        scale_color_brewer(name, palette = palette, drop = drop) +
        guides(colour = guide_legend(name, override.aes = list(size = (min_size + max_size) / 2)))

  }
  else {

    p = p + geom_tile(aes(fill = value), colour = "white")

    if(is.null(nbreaks) & limits)
      p = p + scale_fill_gradient2(name, low = low, mid = mid, high = high, midpoint = midpoint,
                                   limits = c(-1, 1))
    else if(is.null(nbreaks))
      p = p + scale_fill_gradient2(name, low = low, mid = mid, high = high, midpoint = midpoint)
    else
      p = p + scale_fill_brewer(name, palette = palette, drop = drop)

  }

  # add coefficient text
  if(label) {
    if(label_alpha) {
      p = p +
        geom_text(data = subset(D, value != 0),
                  aes(row, variable, label = value, alpha = abs(as.numeric(value))),
                  color = label_color, show_guide = FALSE)
    }
    else {
      p = p +
        geom_text(data = subset(D, value != 0),
                  aes(row, variable, label = value),
                  color = label_color)
    }
  }

  # add diagonal and options
  p = p  +
    geom_text(data = diag, aes(label = variable), ...) +
    scale_x_discrete(breaks = NULL, limits = levels(M$row)) +
    scale_y_discrete(breaks = NULL, limits = levels(M$variable)) +
    labs(x = NULL, y = NULL) +
    coord_equal() +
    po.nopanel

  return(p)
}
NULL


#' Plot ellipses
#'
#'plot.ellipses(-49.2874025,-25.4951519,50)
plot.ellipses <- function(x, y, r) {
  angles <- seq(0,2*pi,length.out=360)
  lines(r*cos(angles)+x,r*sin(angles)+y)
}
NULL




#' Plot circles using lat-long coordinates
#'
#' Plots a circle using latitude and longitude in decimal degrees of the center of the circle.
#' @param lon The longitude in decimal degrees.
#' @param lat The latitude in decimal degrees of the center of the circle.
#' @param radius The radius of the circle.
#' @param col The color to be used.
#' @param in.miles A logical parameter. If TRUE, miles will be used for calculations. Default is in kilometers.
#' @examples
#' map("worldHires", region="brazil")#draw a map of Brazil.
#' curitiba <- mapproject(-49.2874025,-25.4951519) #coordinates of Curitiba
#' points(curitiba, pch=20,col='blue',cex=2)
#' plotCircle(-49.2874025,-25.4951519,150)
#'
#' @export
plotCircle <- function(lon, lat, radius, col="red", in.miles=FALSE){
  if(in.miles){
  earth_radius <- 3959 #Mean Earth radius in miles
  }else{
  earth_radius <- 6371 #Mean Earth radius in kilometers.
  }
  angles_in_degrees <- seq(1:360)
  Lat1Rad <- lat*(pi/180)#Latitude of the center of the circle in radians
  Lon1Rad <- lon*(pi/180)#Longitude of the center of the circle in radians
  angles_in_radians <- angles_in_degrees*(pi/180)#angles in radians
  Lat2Rad <-asin(sin(Lat1Rad)*cos(radius/earth_radius)+cos(Lat1Rad)*sin(radius/earth_radius)*cos(angles_in_radians)) #Latitude of each point of the circle rearding to angle in radians
  Lon2Rad <- Lon1Rad+atan2(sin(angles_in_radians)*sin(radius/earth_radius)*cos(Lat1Rad),cos(radius/earth_radius)-sin(Lat1Rad)*sin(Lat2Rad))#Longitude of each point of the circle rearding to angle in radians
  Lat2Deg <- Lat2Rad*(180/pi)#Latitude of each point of the circle rearding to angle in degrees (conversion of radians to degrees deg = rad*(180/pi) )
  Lon2Deg <- Lon2Rad*(180/pi)#Longitude of each point of the circle rearding to angle in degrees (conversion of radians to degrees deg = rad*(180/pi) )
  polygon(Lon2Deg,Lat2Deg,lty=2, col=fade(col), border=NULL)
}
NULL



