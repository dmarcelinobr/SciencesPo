#' @title Plot a lineplot
#' @description Handy function for making a line plot with R package ggplot.
#' @param data The data frame.
#' @param xvar The name of column containing x variable. Default value is \code{NULL}.
#' @param yvar The name of column containing y variable.
#' @param group.var The name of column containing group variable. This variable is used to color plot according to the group.
#' @param add.point if \code{TRUE}, points are added to the plot. Default value is \code{FALSE}.
#' @param point.size  To modify the point size.
#' @param  point.shape To modify the point shape.
#' @param  point.fill To modify the point fill option.
#' @param  point.color To modify the color of the points.
#'
#' @param arrow To add arrow to a line, usage: arrow=arrow(). See \code{?grid::arrow} for more details. Default value is \code{NULL}.
#' @param x.type Indicate the type of x-axis, either \code{"categorical"} or  \code{"continuous"}. Default value is \code{"categorical"}. When the variable on the x-axis is numeric, it is sometimes useful to treat it as continuous, and sometimes useful to treat it as categorical.
#' @param group.color Color of groups. \code{group.color} should have the same length as groups.
#' @param palette this can be also used to indicate group colors. In this case the parameter group.color should be \code{NULL}.
#' @param \dots Other parameters passed on to ggplot2.customize function.
#' @examples
#' data(Presidents)
#' line.plot(Presidents, election, winner.height)
#' @export
#'
line.plot<-function(data, xvar, yvar,  group.var=NULL, add.point=FALSE, point.size=1.5, point.shape=19, point.fill=NULL, point.color="black", arrow=NULL, x.type=c("categorical", "continuous"), group.color=NULL, palette=NULL,...)
{

  if(!is.null(arrow))
  data=data.frame(data)
  if(x.type[1]=="categorical") data[,xvar]=factor(data[,xvar])
  else if(x.type[1]=="continuous") data[,xvar] = as.numeric(data[,xvar])
  #basic graphs
  if(is.null(group.var)){
    p<-ggplot2::ggplot(data=data, ggplot2::aes_string(x=xvar, y=yvar, group=1))
    p<-p+ggplot2::geom_line(arrow=arrow,...)
    if(is.null(point.fill)) point.fill="white"
    if(add.point) p<-p+ggplot2::geom_point(colour=point.color, size=point.size,shape=point.shape, fill=point.fill)
  }
  # graphs with groups: Set color/shape by another variable
  else{
    data[,group.var]=factor(data[,group.var])#transform group.var to factor
    p<-ggplot2::ggplot(data=data, ggplot2::aes_string(x=xvar, y=yvar,group=group.var, color=group.var,linetype=group.var, shape=group.var, fill=group.var ))
    p<-p+ggplot2::geom_line(arrow=arrow,...)
    if(add.point){
      if(is.null(point.fill)) p<-p+ggplot2::geom_point(size=point.size,...)
      else p<-p+ggplot2::geom_point(size=point.size, fill=point.fill,...)
    }
  }
  #group colors
  if(!is.null(group.color)){
    p<-p+ggplot2::scale_fill_manual(values=group.color)
    p<-p+ggplot2::scale_colour_manual(values=group.color)
  }
  else if(!is.null(palette)){
    p<-p+ggplot2::scale_fill_brewer(palette=palette)
    p<-p+ggplot2::scale_colour_brewer(palette=palette, guide="none")
  }
  #ggplot2.customize : titles, colors, background, legend, ....
  p<-ggplot2.customize(p,...)
  p
}

