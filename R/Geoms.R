#' @encoding UTF-8
#' @title Lollipop charts
#'
#' @description The lollipop geom is used to create lollipop charts.
#' Lollipop charts are the creation of Andy Cotgreave going back to 2011. They
#' are a combination of a thin segment, starting at with a dot at the top and are a
#' suitable alternative to or replacement for bar charts.
#'
#' @details
#' Use the \code{horizontal} parameter to abate the need for \code{coord_flip()}
#' (see the \code{Arguments} section for details).
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "point")}
#' @inheritParams ggplot2::layer
#'
#' @param na.rm  If \code{FALSE} (the default), removes missing values with
#'  a warning. If \code{TRUE} silently removes missing values.
#' @param ... other arguments passed on to \code{\link{layer}}. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like
#'   \code{color = "red"} or \code{size = 3}. They may also be parameters
#'   to the paired geom/stat.
#' @param horizontal If is \code{FALSE} (the default), the function
#'   will draw the lollipops up from the X axis (i.e. it will set \code{xend}
#'   to \code{x} & \code{yend} to \code{0}). If \code{TRUE}, it wiill set
#'   \code{yend} to \code{y} & \code{xend} to \code{0}). Make sure you map the
#'   \code{x} & \code{y} aesthetics accordingly. This parameter helps avoid
#'   the need for \code{coord_flip()}.
#'
#' @param point.size the size of the point
#' @param point.colour the colour of the point
#' @param stalk.size the size of the stem
#' @param stalk.colour the colour of the stem
#' @inheritParams ggplot2::layer
#' @export
#' @examples
#'
#' df <- data.frame(trt=LETTERS[1:10],
#'                  value=seq(100, 10, by=-10))
#'
#' ggplot(df, aes(trt, value)) + geom_lollipop()
#'
#' ggplot(df, aes(value, trt)) + geom_lollipop(horizontal=TRUE)
`geom_lollipop` <- function(mapping = NULL, data = NULL, ...,
                          horizontal = FALSE,
                          point.colour = NULL, point.size = NULL,
                          stalk.colour = NULL, stalk.size = NULL,
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

  layer(
    data = data,
    mapping = mapping,
    stat = "identity",
    geom = GeomLollipop,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      horizontal = horizontal,
      point.colour = point.colour,
      point.size = point.size,
      stalk.colour = stalk.colour,
      stalk.size = stalk.size,
      ...
    )
  )
}


#' Geom Proto
#' @rdname SciencesPo-ggproto
#' @format NULL
#' @usage NULL
#' @export
`GeomLollipop` <- ggproto("GeomLollipop", Geom,
                        required_aes = c("x", "y"),
                        non_missing_aes = c("size", "shape", "point.colour", "point.size", "stalk.colour", "stalk.size", "horizontal"),
                        default_aes = aes(
                          shape = 19, colour = "black", size = 0.5, fill = NA,
                          alpha = NA, stroke = 0.5
                        ),

                        setup_data = function(data, params) {
                          if (params$horizontal) {
                            transform(data, yend = y, xend = 0)
                          } else {
                            transform(data, xend = x, yend = 0)
                          }
                        },

draw_group = function(data, panel_scales, coord,
point.colour = NULL, point.size = NULL,
stalk.colour = NULL, stalk.size = NULL,
horizontal = FALSE) {
points <- data
points$colour <- point.colour %||% data$colour
points$size <- point.size %||% (data$size * 2.5)
stalk <- data
stalk$colour <- stalk.colour %||% data$colour
stalk$size <- stalk.size %||% (data$size * .75)

grid::gList(
ggplot2::GeomSegment$draw_panel(stalk, panel_scales, coord),
ggplot2::GeomPoint$draw_panel(points, panel_scales, coord)
)

},

 draw_key = draw_key_point
)
NULL





#' @encoding UTF-8
#' @title Dumbell charts
#'
#' @description The dumbbell geom is used to create dumbbell charts.
#' Dumbbell dot plots — dot plots with two or more series of data — are an
#' alternative to the clustered bar chart or slope graph.
#'
#' @section Aesthetics:
#' \Sexpr[results=rd,stage=build]{ggplot2:::rd_aesthetics("geom", "segment")}
#' @inheritParams ggplot2::layer
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#'    a warning.  If \code{TRUE} silently removes missing values.
#' @param ... other arguments passed on to \code{\link{layer}}. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like
#'   \code{color = "red"} or \code{size = 3}. They may also be parameters
#'   to the paired geom/stat.
#' @param point.size.l the size of the left point
#' @param point.colour.l the colour of the left point
#' @param point.size.r the size of the right point
#' @param point.colour.r the colour of the right point
#' @inheritParams ggplot2::layer
#' @export
#' @examples
#' df <- data.frame(trt=LETTERS[1:5],
#'                  l=c(20, 40, 10, 30, 50),
#'                  r=c(70, 50, 30, 60, 80))
#'
#' ggplot(df, aes(y=trt, x=l, xend=r)) + geom_dumbbell()
#'
#' ggplot(df, aes(y=trt, x=l, xend=r)) +
#' geom_dumbbell(color="#a3c4dc", size=0.75, point.colour.l="#0e668b")

`geom_dumbbell` <- function(mapping = NULL, data = NULL, ...,
                          point.colour.l = NULL, point.size.l = NULL,
                          point.colour.r = NULL, point.size.r = NULL,
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

  layer(
    data = data,
    mapping = mapping,
    stat = "identity",
    geom = GeomDumbbell,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      point.colour.l = point.colour.l,
      point.size.l = point.size.l,
      point.colour.r = point.colour.r,
      point.size.r = point.size.r,
      ...
    )
  )
}


#' @rdname SciencesPo-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomDumbbell <- ggproto("GeomDumbbell", Geom,
                        required_aes = c("x", "xend", "y"),
                        non_missing_aes = c("size", "shape",
                                            "point.colour.l", "point.size.l",
                                            "point.colour.r", "point.size.r"),
                        default_aes = aes(
                          shape = 19, colour = "black", size = 0.5, fill = NA,
                          alpha = NA, stroke = 0.5
                        ),

                        setup_data = function(data, params) {
                          transform(data, yend = y)
                        },

                        draw_group = function(data, panel_scales, coord,
                                              point.colour.l = NULL, point.size.l = NULL,
                                              point.colour.r = NULL, point.size.r = NULL) {

                          points.l <- data
                          points.l$colour <- point.colour.l %||% data$colour
                          points.l$size <- point.size.l %||% (data$size * 2.5)

                          points.r <- data
                          points.r$x <- points.r$xend
                          points.r$colour <- point.colour.r %||% data$colour
                          points.r$size <- point.size.r %||% (data$size * 2.5)

                          grid::gList(
                            ggplot2::GeomSegment$draw_panel(data, panel_scales, coord),
                            ggplot2::GeomPoint$draw_panel(points.l, panel_scales, coord),
                            ggplot2::GeomPoint$draw_panel(points.r, panel_scales, coord)
                          )

                        },

                        draw_key = draw_key_point
)
NULL





#' @encoding UTF-8
#' @rdname SciencesPo-ggproto
#' @format NULL
#' @usage NULL
#' @export
`GeomSpotlight` <- ggproto("GeomSpotlight", Geom,
                        required_aes = c("x", "y"),
                        default_aes = aes(colour = "black",
                                          linetype=1,
                                          size=1,
                                          s_shape=0.5,  ## corresponds to default shape in xspline of -0.5
                                          s_open=FALSE,
                                          expand=0.05,
                                          spread=0.1),
                        draw_key = draw_key_point,

                        draw_group = function(data, panel_scales, coord) {
                          coords <- coord$transform(data, panel_scales)
                          first_row <- coords[1, , drop = FALSE]
                          rownames(first_row) <- NULL ## prevent warning later

                          m <- lapply(coords[,c("x","y")],mean,na.rm=TRUE)
                          ch <- grDevices::chull(coords[c("x","y")])

                          mkcoords <- function(x,y) {
                            data.frame(x,y,first_row[!names(first_row) %in% c("x","y")])
                          }

                          coords <- coords[ch,]
                          ## FIXME: using grid:: a lot. importFrom instead?

                          ## convert from lengths to physical units, for computing *directions*
                          cc <- function(x,dir="x")
                            grid::convertUnit(grid::unit(x,"native"),"mm",typeFrom="dimension",
                                              axisFrom=dir,valueOnly=TRUE)

                          ## convert back to native (e.g. native + snpc offset)
                          cc_inv <- function(x,dir="x")
                            grid::convertUnit(x,"native",typeFrom="location",
                                              axisFrom=dir,valueOnly=TRUE)

                          cc_comb <- function(x1,x2,dir="x")
                            cc_inv(unit(x1,"native")+unit(x2,"snpc"),dir=dir)

                          ## find normalized vector: d1 and d2 have $x, $y elements
                          normFun <- function(d1,d2) {
                            dx <- cc(d1$x-d2$x)
                            dy <- cc(d1$y-d2$y)
                            r <- sqrt(dx*dx+dy*dy)
                            list(x=dx/r,y=dy/r)
                          }

                          if (nrow(coords)==1) {
                            ## only one point: make a diamond by spreading points vertically
                            ## and horizontally
                            coords <- with(coords,
                                           mkcoords(
                                             c(x,x+spread,x,x-spread),
                                             c(y+spread,y,y-spread,y)))
                          } else if (nrow(coords)==2) {
                            ## only two points: make a diamond by spreading points perpendicularly
                            rot <- matrix(c(0,1,-1,0),2)
                            dd <- c(rot %*% unlist(normFun(coords[1,],coords[2,])))*
                              coords$spread
                            coords <- with(coords, {
                              ## figure out rotated values, then convert *back* to native units
                              ## already in scaled units, so ignore?
                              x <- c(x[1],
                                     m$x+dd[1], ## cc_comb(m$x,dd[1]),
                                     x[2],
                                     m$x-dd[1]) ## cc_comb(m$x,-dd[1]))
                              y <- c(y[1],
                                     m$y+dd[2], ## cc_comb(m$y,dd[2],"y"),
                                     y[2],
                                     m$y-dd[2]) ## cc_comb(m$y,-dd[2],"y"))
                              mkcoords(x,y)
                            })
                          }

                          disp <- normFun(coords,m)

                          grid::xsplineGrob(
                            with(coords,unit(x,"npc")+disp$x*unit(expand,"snpc")),
                            with(coords,unit(y,"npc")+disp$y*unit(expand,"snpc")),
                            ## coords$x,
                            ## coords$y,
                            shape = coords$s_shape-1,  ## kluge!
                            open = first_row$s_open,
                            gp = with(first_row,
                                      grid::gpar(col = colour, lty=linetype))
                          )
                        }
)

if (FALSE) {
  library("grid")
  library("gridBase")
  coords <- data.frame(x=c(1,1),y=c(1,2)*100,spread=c(0.1,0.1))
  plot(y~x,data=d,xlim=c(0,3),ylim=c(0,300))
  vps <- baseViewports()
  pushViewport(vps$inner)
  pushViewport(vps$figure)
  pushViewport(vps$plot)
  ## check that we're in the right place
  m <- as.list(colMeans(coords))
  grid.points(m$x,m$y,gp=gpar(col="red"))
  cc <- function(x,dir="x")
    grid::convertUnit(grid::unit(x,"native"),"mm",typeFrom="dimension",
                      axisFrom=dir,valueOnly=TRUE)
  cc_inv <- function(x,dir="x")
    grid::convertUnit(x,"native",typeFrom="location",
                      axisFrom=dir,valueOnly=TRUE)

  cc_comb <- function(x1,x2,dir="x")
    cc_inv(unit(x1,"native")+unit(x2,"snpc"),dir=dir)

  ## find normalized vector: d1 and d2 have $x, $y elements
  normFun <- function(d1,d2) {
    dx <- cc(d1$x-d2$x)
    dy <- cc(d1$y-d2$y)
    r <- sqrt(dx*dx+dy*dy)
    list(x=dx/r,y=dy/r)
  }

  dd <- c(rot %*% unlist(normFun(coords[1,],coords[2,])))*
    coords$spread
  z <- with(coords, {
    ## figure out rotated values, then convert *back* to native units
    x <- c(x[1],
           cc_comb(m$x,dd[1]),
           x[2],
           cc_comb(m$x,-dd[1]))
    y <- c(y[1],
           cc_comb(m$y,dd[2],"y"),
           y[2],
           cc_comb(m$y,-dd[2],"y"))
    list(x=x,y=y)
  })
  with(z,grid.points(x,y,gp=gpar(col="blue")))

  print(grid::convertWidth(unit(1,'npc'),'native'))
  print(grid::convertHeight(unit(1,'npc'),'native'))

}

#' Automatically enclose points in a polygon
#'
#' @param mapping mapping
#' @param data  data
#' @param stat  stat
#' @param position position
#' @param na.rm na.rm
#' @param show.legend  show.legend
#' @param inherit.aes inherit.aes
#' @param ...  dots
#' @return adds a circle around the specified points
#' @author Ben Bolker
#' @export
#' @examples
#' d <- data.frame(x=c(1,1,2),y=c(1,2,2)*100)
#'
#' gg <- ggplot(d,aes(x,y))
#' gg <- gg + scale_x_continuous(expand=c(0.5,1))
#' gg <- gg + scale_y_continuous(expand=c(0.5,1))
#'
#' gg + geom_spotlight(s_shape=1, expand=0) + geom_point()
#'
#' gg + geom_spotlight(s_shape=0.5, expand=0.1, colour="purple") + geom_point()
#'
#' gg + geom_spotlight(data=subset(d, x==1), colour="red", spread=0.02) +
#'   geom_point()
#'
#' gg <- ggplot(mpg, aes(displ, hwy))
#' gg + geom_spotlight(data=subset(mpg, hwy>40)) + geom_point()
#'
#' ss <- subset(mpg,hwy>31 & displ<2)
#'
#' gg + geom_spotlight(data=ss, colour="blue", s_shape=0.9, expand=0.07) +
#'   geom_point() + geom_point(data=ss, colour="blue")
#'
`geom_spotlight` <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE, ...) {
  layer(
    geom = GeomSpotlight, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
NULL

