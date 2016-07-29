#' @include SciencesPo.R
#' @include Palettes.R
NULL



#' @encoding UTF-8
#' @title SciencesPo Base Fonts
#' @description Used to ascertain required theme fonts.
#' @author
#' \Sexpr[stage=build,results=rd]{tools:::Rd_package_author("SciencesPo")}
#' @keywords ggplot2
#' @export
`SciencesPoFont` <- function(){
  if(IsExtrafontInstalled()){
    loadNamespace("extrafont")
    themesFont <- extrafont::choose_font(c("Gill Sans MT", "Gill Sans", "GillSans", "Verdana", "serif", "Tahoma"), quiet = FALSE)
  }else{
    themesFont <- "Helvetica"
  }
  return(themesFont)
}
NULL




#' @title Plot Customizing
#' @description Several wrapper functions to deal with ggplot2.
#' @param angle the angle of rotation.
#' @param position the location of the legend ('top', 'bottom', 'right', 'left' or 'none').
#' @author
#' \Sexpr[results=rd, stage=build]{tools:::Rd_package_author("SciencesPo")}
#' @keywords ggplot2
#' @export
#' @name Plotting
#' @examples
#' Previewplot()
`Previewplot` <- function () {
  data <- data.frame(
    x = factor(c('A', 'B', 'C', 'A', 'B', 'C')),
    y = seq(10, 30, 10),
    check.names = FALSE
  )
  ggplot(data, aes_string(x = 'x', y = 'y')) +
    geom_blank() +
    scale_y_continuous(
      labels = function(x)
        format(x, scientific = FALSE, trim = TRUE)
    ) +
    labs(x = 'x-axis title', y = 'y-axis title', title = 'Plot Title')
}


#' @keywords ggplot2
#' @export
#' @rdname Plotting
`PreviewTheme`<- Previewplot
NULL



#' @keywords ggplot2
#' @name Plotting
#' @export
`align_title_left` <- function ()
  theme(plot.title = element_text(hjust = 0))



#' @rdname Plotting
#' @keywords ggplot2
#' @export
`align_title_right` <- function ()
  theme(plot.title = element_text(hjust = 1))



#' @rdname Plotting
#' @keywords ggplot2
#' @export
`no_title` <- function ()
  theme(plot.title = element_blank())


#' @rdname Plotting
#' @keywords ggplot2
#' @export
`no_y_gridlines` <- function()
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())


#' @rdname Plotting
#' @keywords ggplot2
#' @export
`no_x_gridlines` <- function ()
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())


#' @rdname Plotting
#' @keywords ggplot2
#' @export
`no_major_x_gridlines` <- function ()
  theme(panel.grid.major.x = element_blank())


#' @rdname Plotting
#' @keywords ggplot2
#' @export
`no_major_y_gridlines` <- function ()
  theme(panel.grid.major.y = element_blank())

#' @rdname Plotting
#' @keywords ggplot2
#' @export
`no_major_gridlines` <- function ()
  theme(panel.grid.major = element_blank())


#' @rdname Plotting
#' @keywords ggplot2
#' @export
`no_minor_x_gridlines` <- function ()
  theme(panel.grid.minor.x = element_line())


#' @rdname Plotting
#' @keywords ggplot2
#' @export
`no_minor_y_gridlines` <- function ()
  theme(panel.grid.minor.y = element_blank())



#' @rdname Plotting
#' @keywords ggplot2
#' @export
`no_minor_gridlines` <- function ()
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())



#' @rdname Plotting
#' @keywords ggplot2
#' @export
`no_gridlines` <- function ()
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())



#' @rdname Plotting
#' @keywords ggplot2
#' @export
`rotate_axes_text` <- function (angle)
  theme(axis.text = element_text(angle = angle, hjust = 1L))



#' @rdname Plotting
#' @keywords ggplot2
#' @export
`rotate_x_text` <- function (angle)
  theme(axis.text.x = element_text(angle = angle, hjust = 1L))


#' @rdname Plotting
#' @keywords ggplot2
#' @export
`rotate_y_text` <- function (angle)
  theme(axis.text.y = element_text(angle = angle, hjust = 1L))




#' @rdname Plotting
#' @keywords ggplot2
#' @export
`no_axes` <- function()
  theme(axis.line = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())



#' @rdname Plotting
#' @keywords ggplot2
#' @export
`no_x_axis` <- function()
  theme(axis.line.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


#' @rdname Plotting
#' @keywords ggplot2
#' @export
`no_y_axis` <- function()
  theme(axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())



#' @rdname Plotting
#' @keywords ggplot2
#' @export
`no_x_line` <- function()
  theme(axis.line.x = element_blank())



#' @rdname Plotting
#' @keywords ggplot2
#' @export
`no_y_line` <- function()
  theme(axis.line.y = element_blank())



#' @rdname Plotting
#' @keywords ggplot2
#' @export
`no_x_text` <- function ()
  theme(axis.text.x = element_blank())



#' @rdname Plotting
#' @keywords ggplot2
#' @export
`no_y_text` <- function ()
  theme(axis.text.y = element_blank())



#' @rdname Plotting
#' @keywords ggplot2
#' @export
`no_ticks` <- function ()
  theme(axis.ticks = element_blank())



#' @rdname Plotting
#' @keywords ggplot2
#' @export
`no_x_ticks` <- function ()
  theme(axis.ticks.x = element_blank())



#' @rdname Plotting
#' @keywords ggplot2
#' @export
`no_y_ticks` <- function ()
  theme(axis.ticks.y = element_blank())



#' @rdname Plotting
#' @keywords ggplot2
#' @export
`no_axes_titles` <- function () theme(axis.title = element_blank())



#' @rdname Plotting
#' @keywords ggplot2
#' @export
`no_x_title` <- function () theme(axis.title.x = element_blank())



#' @rdname Plotting
#' @keywords ggplot2
#' @export
`no_y_title` <- function() theme(axis.title.y = element_blank())



#' @rdname Plotting
#' @keywords ggplot2
#' @export
`move_legend` <- function(position)
  theme(legend.position = position)


#' @rdname Plotting
#' @keywords ggplot2
#' @export
`legend_bottom` <- function () move_legend('bottom')


#' @rdname Plotting
#' @keywords ggplot2
#' @export
`legend_top` <- function () move_legend('top')


#' @rdname Plotting
#' @keywords ggplot2
#' @export
`legend_left` <- function () move_legend('left')


#' @rdname Plotting
#' @keywords ggplot2
#' @export
`legend_right` <- function () move_legend('right')


#' @rdname Plotting
#' @keywords ggplot2
#' @export
`no_legend` <- function () move_legend('none')


#' @rdname Plotting
#' @keywords ggplot2
#' @export
`no_legend_title` <- function () theme(legend.title = element_blank())
NULL





#' @encoding UTF-8
#' @title The Default Theme
#'
#' @description After loading the SciencesPo package, this theme will be
#' set to default for all subsequent graphs made with ggplot2.
#'
#' @param legend enables to set legend position, default is "bottom".
#' @param base_family a name for default font family.
#' @param base_size overall font size. Default is 14.
#' @param horizontal logical. Horizontal axis lines?
#' @param axis_line enables to set x and y axes.
#' @family themes pub
#' @author
#' \Sexpr[stage=build,results=rd]{tools:::Rd_package_author("SciencesPo")}
#' @return The theme.
#' @keywords ggplot2
#' @seealso \code{\link[ggplot2]{theme}}, \code{\link{theme_538}}, \code{\link{theme_blank}}.
#' @examples
#' Previewplot() + theme_pub()
#'
#' # Anscombe data
#' dat <- data.frame()
#' for(i in 1:4)
#' dat <- rbind(dat, data.frame(set=i, x=anscombe[,i], y=anscombe[,i+4]))
#'
#'  gg <- ggplot(dat, aes(x, y))
#' gg <- gg + geom_point(size=5, color="red", fill="orange", shape=21)
#' gg <- gg + geom_smooth(method="lm", fill=NA, fullrange=TRUE)
#' gg <- gg + facet_wrap(~set, ncol=2)
#' gg <- gg + theme_pub(base_family=SciencesPoFont())
#' gg <- gg + theme(plot.background=element_rect(fill="#f7f7f7"))
#' gg <- gg + theme(panel.background=element_rect(fill="#f7f7f7"))
#'
#' @export
`theme_pub` <- function(legend = 'bottom',
                      base_size = 12,
                      base_family = "",
                      horizontal = FALSE,
                      axis_line = FALSE) {
  half_line <- base_size / 2
  theme(
    # Elements in this first block aren't used directly, but are inherited
    # by others
    line = element_line(
      colour = "#525252",
      size = 0.5,
      linetype = 1,
      lineend = "butt"
    ),
    rect = element_rect(
      fill = "transparent",
      colour = NA,
      size = 0.5,
      linetype = 1
    ),
    text =  element_text(
      family = base_family,
      face = "plain",
      colour = "#1e1e1e",
      size = base_size,
      lineheight = 0.9,
      hjust = 0.5,
      vjust = 0.5,
      angle = 0,
      margin = ggplot2::margin(),
      debug = FALSE
    ),

    axis.line =          if (axis_line) {element_line()
    } else{
      element_blank()
    },
    axis.text =          element_text(
      size = rel(0.95),
      face = "plain",
      colour = "#1e1e1e"),
    axis.text.x =        element_text(
      margin = ggplot2::margin(t = 0.8 * half_line / 2),
      vjust = 1
    ),
    axis.text.y = element_text(
      margin = ggplot2::margin(r = 0.8 * half_line / 2),
      hjust = 1
    ),
    axis.ticks = element_line(),
    axis.ticks.length =  grid::unit(half_line/2, "pt"),
    axis.title = element_text(size = rel(0.90), face = "plain"),
    axis.title.x = element_text(margin = ggplot2::margin(
      t = 0.8 * half_line, b = 0.8 * half_line / 2
    )),
    axis.title.y = element_text( angle = 90,
      margin = ggplot2::margin(r = 0.8 * half_line, l = 0.8 * half_line / 2)
    ),
    legend.background =  element_rect(colour = NA),
    legend.margin =      grid::unit(0.2, "cm"),
    legend.key =         element_rect(colour = NA),
    legend.key.size =    grid::unit(1.2, "lines"),
    legend.key.height =  NULL,
    legend.key.width =   NULL,
    legend.text =        element_text(size = rel(0.8)),
    legend.text.align =  NULL,
    legend.title =       element_text(hjust = 0),
    legend.title.align = NULL,
    legend.position =    legend,
    legend.direction =   NULL,
    legend.justification = "center",
    legend.box =         NULL,
    panel.background =   element_blank(),
    panel.border =       element_blank(),
    panel.grid.major.y =   element_line(colour = "grey90"),
    panel.grid.major.x =   element_line(colour = "grey90"),
    panel.grid.minor.y =   element_line(colour = "grey90", size = 0.25),
    panel.grid.minor.x =   element_line(colour = "grey90", size = 0.25),
    panel.margin =       grid::unit(half_line, "pt"),
    panel.margin.x =     NULL,
    panel.margin.y =     NULL,
    panel.ontop    =     FALSE,
    strip.background =   element_rect(fill = "#DADADA", colour = NA),
    strip.text = element_text(size = rel(0.80), face = "bold", colour = "#282828"),
    strip.text.x = element_text(margin = ggplot2::margin(t = half_line, b = half_line)),
    strip.text.y = element_text( angle = -90, margin = ggplot2::margin(l = half_line, r = half_line)),
    strip.switch.pad.grid = grid::unit(0.1, "cm"),
    strip.switch.pad.wrap = grid::unit(0.1, "cm"),
    plot.background = element_rect(colour = "transparent"),
    plot.title = element_text(size = rel(1.2), face = "bold", colour = "#141414", hjust = 0, margin = ggplot2::margin(b = half_line * 1.1)
    ),
  #  plot.subtitle = element_text(size = rel(0.85), hjust = 0, margin = margin(b = half_line * 0.9)),

  #  plot.caption = element_text(size = rel(0.9), hjust = 1, margin = margin(b = half_line * 0.9)),

plot.margin =  margin(half_line, half_line, half_line, half_line),
    complete = TRUE)
}
NULL




#' @title Themes for ggplot2 Graphs
#'
#' @description  Theme for plotting  with ggplot2.
#'
#' @param legend enables to set legend position, default is "none".
#' @param legend_title will the legend have a title? Defaults is \code{FALSE}.
#' @param base_family a nmae for default font family.
#' @param base_size overall font size. Default is 13.
#' @param horizontal logical. Horizontal axis lines?
#' @param colors default colors used in the plot in the following order: background, lines, text, and title.
#' @family themes 538
#' @keywords ggplot2
#' @return The theme.
#' @author
#' \Sexpr[stage=build,results=rd]{tools:::Rd_package_author("SciencesPo")}
#' @examples
#' qplot(1:10, (1:10)^3) + theme_fte()
#'
#'
#' mycolors = c("wheat",  "#C2AF8D",  "#8F6D2F", "darkred")
#' qplot(1:10, (1:10)^3) +
#'  theme_fte(colors=mycolors)
#'
#' # Check that it is a complete theme
#' attr(theme_fte(), "complete")
#' @export
#' @aliases theme_538
`theme_fte` <- function(legend = 'none',
                        legend_title = FALSE,
                        base_size = 12,
                        horizontal = TRUE,
                        base_family = '',
                        colors = c('#F0F0F0', '#D9D9D9', '#60636A', '#525252')) {
  half_line <- base_size / 2
  theme(
    # Elements in this first block aren't used directly, but are inherited
    # by others
    line = element_line(
      colour = colors[2],
      size = 0.5,
      linetype = 1,
      lineend = "butt"
    ),
    rect = element_rect(
      fill = colors[1],
      colour = colors[1],
      size = 0.5,
      linetype = 1
    ),
    text = element_text(
      family = base_family,
      face = "bold",
      colour = colors[3],
      size = base_size,
      lineheight = 1,
      hjust = 0.5,
      vjust = 0.5,
      angle = 0,
      margin = ggplot2::margin(),
      debug = FALSE
    ),

    axis.line =          element_blank(),
    axis.text =          element_text(size = rel(1)),
    axis.text.x =        element_text(
      margin = ggplot2::margin(t = 0.8 * half_line / 2),
      vjust = 1,
      size = rel(0.85)
    ),
    axis.text.y =        element_text(
      margin = ggplot2::margin(r = 0.8 * half_line / 2),
      hjust = 1,
      size = rel(0.85)
    ),
    axis.ticks.y =        element_line(color = colors[2]),
    axis.ticks.x =        element_line(color = colors[2]),
    axis.ticks.length =  grid::unit(half_line / 2, "pt"),
    axis.title =          element_text(size = rel(0.85), color = colors[3]),
    axis.title.x =       element_text(
      vjust = 0,
      margin = ggplot2::margin(t = 0.8 * half_line, b = 0.8 * half_line / 2)
    ),
    axis.title.y =       element_text(
      angle = 90,
      vjust = 1.25,
      margin = ggplot2::margin(r = 0.8 * half_line, l = 0.8 * half_line / 2)
    ),

    legend.background =  element_rect(linetype = 0),
    legend.margin =      grid::unit(0.2, "cm"),
    legend.key =         element_rect(linetype = 0),
    legend.key.size =    grid::unit(1.2, "lines"),
    legend.key.height =  NULL,
    legend.key.width =   NULL,
    legend.text =        element_text(size = rel(0.85)),
    legend.text.align =  NULL,
    legend.title =       if (legend_title) {
      element_text(size = rel(0.8), hjust = 0)
    } else {
      element_blank()
    },
    legend.title.align = NULL,
    legend.position =    legend,
    legend.direction =   NULL,
    legend.justification = "center",
    legend.box =         NULL,

    panel.background =   element_rect(),
    panel.border =       element_blank(),
    panel.grid.major =   element_line(),
    panel.grid.minor =   element_blank(),
    panel.margin =       grid::unit(half_line, "pt"),
    panel.margin.x =     NULL,
    panel.margin.y =     NULL,
    panel.ontop    =     FALSE,

    strip.background =   element_rect(),
    strip.text =         element_text(
      colour = "grey15",
      face = "plain",
      size = rel(0.8)
    ),
    strip.text.x =       element_text(margin = ggplot2::margin(t = half_line, b = half_line)),
    strip.text.y =       element_text(
      angle = -90,
      margin = ggplot2::margin(l = half_line, r = half_line)
    ),
    strip.switch.pad.grid = grid::unit(0.1, "cm"),
    strip.switch.pad.wrap = grid::unit(0.1, "cm"),

    plot.background =    element_rect(),
    plot.title =         element_text(face = "bold",
      family = "sans",
      size = rel(1.2),
      hjust = 0,
      margin = ggplot2::margin(b = half_line * 1.2)
    ),
    #  plot.subtitle = element_text(size = rel(0.85), hjust = 0, margin = margin(b = half_line * 0.9)),
 # plot.caption = element_text(size = rel(0.9), hjust = 1, margin = margin(b = half_line * 0.9)),
    plot.margin = margin(half_line, half_line, half_line, half_line),
    complete = TRUE
  )
}

#' @export
`theme_538` <- theme_fte





#' @title Create a Completely Empty Theme
#'
#' @description The theme created by this function shows nothing but the plot panel.
#' @inheritParams ggplot2::theme_grey
#' @param base_family a name for default font family.
#' @param base_size overall font size. Default is 12.
#' @param legend the legend position.
#' @family themes blank
#' @return The theme.
#' @keywords ggplot2
#' @author
#' \Sexpr[stage=build,results=rd]{tools:::Rd_package_author("SciencesPo")}
#'
#' @examples
#' # plot with small amount of remaining padding
#' qplot(1:10, (1:10)^2) + theme_blank()
#'
#' # remaining padding removed
#' qplot(1:10, (1:10)^2) + theme_blank() + labs(x = NULL, y = NULL)
#'
#' # Check that it is a complete theme
#' attr(theme_blank(), "complete")
#'
#' @export
`theme_blank` <- function(base_size = 12, base_family = "serif", legend="none") {
ret <- (theme_grey(base_size = base_size, base_family = base_family) +
    theme(
      rect              = element_rect(
        fill = "transparent", colour = NA, color = NA, size = 0, linetype = 0
      ),
      line              = element_blank(),
      text              = element_blank(),
      title             = element_blank(),
      # to debug, uncomment next line
      #plot.background   = element_rect(colour = "blue", fill = "cyan"),
      panel.background  = element_blank(),
      axis.ticks.length = grid::unit(0, "cm"),
      legend.position   = legend,
      panel.margin      = grid::unit(c(0, 0, 0, 0), "cm"),
      plot.margin       = grid::unit(c(0, 0, 0, 0), "cm")
    ))
ret
}
NULL




#' @title The Dark Side Theme
#' @description The dark side of the things.
#' @inheritParams ggplot2::theme_bw
#' @param base_family the name for default font family.
#' @param base_size overall font size. Default is 12.
#' @param legend the position of the legend if any.
#' @family themes darkside
#' @keywords ggplot2
#' @author
#' \Sexpr[stage=build,results=rd]{tools:::Rd_package_author("SciencesPo")}
#' @return The theme.
#' # plot with small amount of padding
#' qplot(1:10, (1:10)^2, color="green") + theme_darkside()
#'
#' # Check that it is a complete theme
#' attr(theme_darkside(), "complete")
#'
#' @export
`theme_darkside` = function(base_size = 12, base_family = "serif", legend="none") {
ret  <- (theme_bw(base_size = base_size) +
    theme(
      text = element_text(family = base_family, color = "grey80"),
      rect = element_rect(fill = "#000000", color = "#000000"),
      #strip.background =   element_rect(fill = "grey30", colour = "grey10"),
      plot.background = element_rect(fill = "#000000", color = "#000000"),
      panel.background = element_rect(fill = "#000000", color = "#000000"),
      legend.background = element_rect(linetype = 0),
      legend.position = legend,
      plot.title = element_text(family = "sans"),
      panel.border = element_blank(),
      panel.grid.major =   element_line(colour = "grey40", size = 0.2),
      panel.grid.minor =   element_line(colour = "grey25", size = 0.5),
      axis.text = element_text(),
      axis.title = element_blank(),
      axis.ticks = element_blank()
    ))
ret
}
NULL




#' @title The ScPo Theme
#' @description The scpo theme for using with ggplot2 objects.
#' @inheritParams ggplot2::theme_bw
#'
#' It requires installing Cabin fonts unless you change the font parameters
#'
#' \url{http://www.impallari.com/cabin/}
#'
#' @param base_family base font family
#' @param base_size base font size
#' @param strip_text_family facet label font family
#' @param strip_text_size facet label text size
#' @param title_family plot tilte family
#' @param title_size plot title font size
#' @param title_margin plot title margin
#' @param margins plot margins
#' @param subtitle_family plot subtitle family
#' @param subtitle_size plot subtitle size
#' @param subtitle_margin plot subtitle margin
#' @param caption_family plot caption family
#' @param caption_size plot caption size
#' @param caption_margin plot caption margin
#' @param axis_title_family axis title font family
#' @param axis_title_size axis title font size
#' @param axis_title_just axis title font justification \code{blmcrt}
#' @param grid panel grid (\code{TRUE}, \code{FALSE}, or a combination of
#'        \code{X}, \code{x}, \code{Y}, \code{y})
#' @param axis axis \code{TRUE}, \code{FALSE}, [\code{xy}]
#' @param ticks ticks
#' @export
#' @return The theme.
#' @examples
#' # plot with small amount of padding
#' # qplot(1:10, (1:10)^2, color="green") + theme_scpo()
#'
#' # Check that it is a complete theme
#' attr(theme_scpo(), "complete")
#'
`theme_scpo` <- function(base_family="",
                           base_size = 11,
                           strip_text_family = base_family,
                           strip_text_size = 12,
                           title_family="",
                           title_size = 18,
                           title_margin = 10,
                           margins = margin(10, 10, 10, 10),
                           subtitle_family="",
                           subtitle_size = 12,
                           subtitle_margin = 15,
                           caption_family="",
                           caption_size = 9,
                           caption_margin = 10,
                           axis_title_family = subtitle_family,
                           axis_title_size = 9,
                           axis_title_just = "rt",
                           grid = TRUE,
                           axis = FALSE,
                           ticks = FALSE) {

  ret <- theme_minimal(base_family=base_family, base_size=base_size)
  ret <- ret + theme(legend.background=element_blank())
  ret <- ret + theme(legend.key=element_blank())
  if (inherits(grid, "character") | grid == TRUE) {
    ret <- ret + theme(panel.grid=element_line(color="#2b2b2bdd", size=0.10))
    ret <- ret + theme(panel.grid.major=element_line(color="#2b2b2b99", size=0.10))
    ret <- ret + theme(panel.grid.minor=element_line(color="#2b2b2b99", size=0.05))

    if (inherits(grid, "character")) {
      if (regexpr("X", grid)[1] < 0) ret <- ret + theme(panel.grid.major.x=element_blank())
      if (regexpr("Y", grid)[1] < 0) ret <- ret + theme(panel.grid.major.y=element_blank())
      if (regexpr("x", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.x=element_blank())
      if (regexpr("y", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.y=element_blank())
    }

  } else {
    ret <- ret + theme(panel.grid=element_blank())
  }

  if (inherits(axis, "character") | axis == TRUE) {
    ret <- ret + theme(axis.line=element_line(color="#2b2b2b", size=0.15))
    if (inherits(axis, "character")) {
      axis <- tolower(axis)
      if (regexpr("x", axis)[1] < 0) {
        ret <- ret + theme(axis.line.x=element_blank())
      } else {
        ret <- ret + theme(axis.line.x=element_line(color="#2b2b2b", size=0.15))
      }
      if (regexpr("y", axis)[1] < 0) {
        ret <- ret + theme(axis.line.y=element_blank())
      } else {
        ret <- ret + theme(axis.line.y=element_line(color="#2b2b2b", size=0.15))
      }
    } else {
      ret <- ret + theme(axis.line.x=element_line(color="#2b2b2b", size=0.15))
      ret <- ret + theme(axis.line.y=element_line(color="#2b2b2b", size=0.15))
    }
  } else {
    ret <- ret + theme(axis.line=element_blank())
  }

  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
    ret <- ret + theme(axis.ticks.x = element_blank())
    ret <- ret + theme(axis.ticks.y = element_blank())
  } else {
    ret <- ret + theme(axis.ticks = element_line(size=0.15))
    ret <- ret + theme(axis.ticks.x = element_line(size=0.15))
    ret <- ret + theme(axis.ticks.y = element_line(size=0.15))
    ret <- ret + theme(axis.ticks.length = grid::unit(5, "pt"))
  }

  xj <- switch(tolower(substr(axis_title_just, 1, 1)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)
  yj <- switch(tolower(substr(axis_title_just, 2, 2)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)

  ret <- ret + theme(axis.text.x=element_text(margin=margin(t=-10)))
  ret <- ret + theme(axis.text.y=element_text(margin=margin(r=-10)))
  ret <- ret + theme(axis.title=element_text(size=axis_title_size, family=axis_title_family))
  ret <- ret + theme(axis.title.x=element_text(hjust=xj, size=axis_title_size, family=axis_title_family))
  ret <- ret + theme(axis.title.y=element_text(hjust=yj, size=axis_title_size, family=axis_title_family))
  ret <- ret + theme(strip.text=element_text(hjust=0, size=strip_text_size, family=strip_text_family))
  ret <- ret + theme(plot.title=element_text(hjust=0, size=title_size, margin=margin(b=title_margin), family=title_family))
  # ret <- ret + theme(plot.subtitle=element_text(hjust=0, size=subtitle_size, margin=margin(b=subtitle_margin), family=subtitle_family))
  ret <- ret + theme(plot.caption=element_text(hjust=1, size=caption_size, margin=margin(t=caption_margin), family=caption_family))
  ret <- ret + theme(plot.margin=margins)
  ret
}
NULL
