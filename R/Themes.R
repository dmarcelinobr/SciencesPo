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
`checkThemeFonts` <- function(){
  if(IsExtrafontInstalled()){
    loadNamespace("extrafont")
    extrafont::font_import()
    themeFonts <- extrafont::choose_font(c("Gill Sans MT", "Gill Sans", "GillSans", "serif", "Verdana", "Tahoma"), quiet = FALSE)
  }else{
    themeFonts <- "Helvetica"
  }
  return(themeFonts)
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
#' preview_ggplot()
`preview_ggplot` <- function () {
  data <- data.frame(
    x = factor(c('A', 'B', 'C', 'A', 'B', 'C')),
    y = seq(10, 30, 10),
    check.names = FALSE
  )
  ggplot(data, aes_string(x = 'x', y = 'y')) +
    geom_blank() +
    scale_y_continuous(
      labels = function(x)
        format(x, scientific = FALSE, trim = TRUE)) +
    labs(x = 'x-axis title', y = 'y-axis title', title = 'Plot Title')
}


#' @keywords ggplot2
#' @export
#' @rdname Plotting
`preview_theme`<- preview_ggplot
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




#' Rescale a ggplot2 plot so that axes follow a given ratio
#'
#' \code{gg_rescale} rescales the given ggplot2 so that the axes follow the
#' given ratio (horizontally). If a plot is not specified, the last plot
#' displayed is used.
#'
#' @export
#' @param plot A ggplot2 plot object. By default, the last plot displayed is
#' used.
#' @param ratio The aspect ratio to use for the axes. This is independent of
#' units used in the plot, so the size of the X axis will be ratio times the
#' total size of the Y axis.
#' @return A modified ggplot2 plot
#' @examples
#' p <- ggplot(mtcars, aes(x = cyl, y = mpg)) +
#'     geom_point(shape = 1)
#' gg_rescale(ratio = 1.67)
#'
gg_rescale <- function(plot = last_plot(), ratio) {
  plot + theme(aspect.ratio = 1 / ratio)
}

#' @export
rescale_plot <- function(plot = last_plot(), ratio) {
  warning("rescale_plot is depricated. Use gg_rescale instead.")
  gg_rescale(plot = plot, ratio = ratio)
}


#' @description \code{gg_rescale_golden} rescales a ggplot2 plot so that axes
#' follow golden ratio
#' @rdname gg_rescale
#' @param orient Whether the golden ratio should be horizontal
#' (\code{h}; default) or vertical (\code{v})
#' @export
#'
gg_rescale_golden <- function(plot = last_plot(), orient = "h") {
  gr <- (1 + sqrt(5)) / 2

  if (orient == "h") {
    gg_rescale(plot = plot, ratio = gr)
  }
  else if (orient == "v") {
    gg_rescale(plot = plot, ratio = 1/gr)
  }
  else {
    stop("Invalid orientation. Must be either 'h' or 'v'")
  }
}

#' @export
rescale_golden <- function(plot = last_plot()) {
  warning("rescale_golden is depricated. Use gg_rescale_golden instead.")
  gg_rescale_golden(plot = plot)
}


#' @description \code{gg_rescale_square} rescales a ggplot2 plot so that its
#' axes are square
#' @rdname gg_rescale
#' @export
#'
gg_rescale_square <- function(plot = last_plot()) {
  gg_rescale(plot = plot, ratio = 1)
}

#' @export
rescale_square <- function(plot = last_plot()) {
  warning("rescale_square is depricated. Use gg_rescale_square instead.")
  gg_rescale_square(plot = plot)
}



#' Save a plot with proportions equal to the golden ratio
#'
#' \code{ggsave_golden} is a wrapper for \code{\link{ggsave}} that saves a
#' ggplot to a file with proportions equal to the golden ratio (wide).
#'
#' @export
#' @param filename The name of the file to be written to
#' @param plot The plot object to save. Defaults to last plot displayed.
#' @param width The width of the resulting image (default: 8)
#' @param height The height of the resulting image (default: 4.94)
#' @param ... Additional parameters to be passed to \code{\link{ggsave}}
#' @importFrom ggplot2 ggsave

`ggsave_golden` <- function(filename, plot=last_plot(), ...) {
  ggsave(filename = filename, plot = plot, width = 8,
         height = 8/1.61803398875, ...)
}




#' @title A Flex Theme
#' @description The SciencesPo flex theme for using with ggplot2 objects.
#' @inheritParams ggplot2::theme_bw
#'
#' @md
#' @param base_family,base_size base font family and size
#' @param plot_title_family,plot_title_face,plot_title_size,plot_title_margin plot title family, face, size and margi
#' @param subtitle_family,subtitle_face,subtitle_size plot subtitle family, face and size
#' @param subtitle_margin plot subtitle margin bottom (single numeric value)
#' @param strip_text_family,strip_text_face,strip_text_size facet label font family, face and size
#' @param caption_family,caption_face,caption_size,caption_margin plot caption family, face, size and margin
#' @param axis_title_family,axis_title_face,axis_title_size axis title font family, face and size
#' @param axis_title_just axis title font justification, one of `[blmcrt]`
#' @param plot_margin plot margin (specify with [ggplot2::margin])
#' @param grid panel grid (\code{TRUE}, \code{FALSE}, or a combination of
#'        \code{X}, \code{x}, \code{Y}, \code{y})
#' @param axis axis \code{TRUE}, \code{FALSE}, [\code{xy}]
#' @param ticks ticks if `TRUE` add ticks
#' @export
#' @return The theme.
#' @examples
#' # plot with small amount of padding
#' qplot(1:10, (1:10)^2) +
#'  theme_flex(grid="Y")
#'
#' qplot(1:10, (1:10)^2) +
#' theme_flex(axis='xy', axis_size=.75)
#'
#' # Check that it is a complete theme
#' # attr(theme_flex(), "complete")
#'
#' \dontrun{
#' library(ggplot2)
#' library(dplyr)
#'
#' # seminal scatterplot
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point() +
#'   labs(x="Fuel effiiency (mpg)", y="Weight (tons)",
#'        title="Seminal ggplot2 scatterplot example",
#'        subtitle="A plot that is only useful for demonstration purposes",
#'        caption="Brought to you by the letter 'g'") +
#'   theme_flex() +
#'   scale_color_flex()
#'
#' # seminal bar chart
#'
#' update_geom_font_defaults()
#'
#' count(mpg, class) %>%
#'   ggplot(aes(class, n)) +
#'   geom_col() +
#'   geom_text(aes(label=n), nudge_y=3) +
#'   labs(x="Fuel efficiency (mpg)", y="Weight (tons)",
#'        title="Seminal ggplot2 bar chart example",
#'        subtitle="A plot that is only useful for demonstration purposes",
#'        caption="Brought to you by the letter 'g'") +
#'   theme_flex(grid="Y") +
#'   theme(axis.text.y=element_blank()) +
#'   scale_color_flex()
#'
#'
#' count(mpg, class) %>%
#' mutate(n=n*2000) %>%
#' arrange(n) %>%
#' mutate(class=factor(class, levels=class)) %>%
#' ggplot(aes(class, n)) +
#' geom_col() +
#' geom_text(aes(label = scales::comma(n)), hjust=0, nudge_y=2000) +
#' scale_y_comma(limits=c(0,150000)) +
#' coord_flip() +
#' labs(x = "Fuel effiiency (mpg)", y = "Weight (tons)",
#'    title="Seminal ggplot2 column chart example with commas",
#'    subtitle="A plot that is only useful for demonstration purposes,
#'    esp since you'd never\nreally want direct labels and axis labels",
#'    caption="Brought to you by the letter 'g'") +
#' theme_flex(grid="X")
#'
#'
#'
#' ggplot(brpopage, aes(x=Year, y=Thousands, fill=AgeGroup)) +
#' geom_area() +
#' scale_fill_flex("colorblind") +
#' scale_x_continuous(expand=c(0,0)) +
#' scale_y_comma() +
#' labs(title="Age distribution of population in Brazil, 1900-2010",
#'    subtitle="Example data from the R SciencesPo Cookbook",
#'       caption="Source: SciencesPo Cookbook") +
#'  theme_flex(grid="XY") +
#'  theme(axis.text.x=element_text(hjust=c(0, 0.5, 0.5, 0.5, 1))) +
#'  theme(legend.position="bottom")
#' }
#'
`theme_flex` <- function(base_family="sans", base_size = 11,
                               plot_title_family=base_family, plot_title_size = 18,
                               plot_title_face="bold", plot_title_margin = 10,
                               subtitle_family=base_family, subtitle_size = 12,
                               subtitle_face = "plain", subtitle_margin = 15,
                               strip_text_family = base_family, strip_text_size = 12,
                               strip_text_face = "plain",
                               caption_family = base_family, caption_size = 9,
                               caption_face = "italic", caption_margin = 10,
                               axis_title_family = subtitle_family, axis_title_size = 9,
                               axis_title_face = "plain", axis_title_just = "rt",
                               plot_background = TRUE,
                               plot_margin = margin(10, 10, 10, 10),
                               grid = TRUE, axis = FALSE, axis_size = 0.15, ticks = FALSE) {

  ret <- ggplot2::theme_minimal(base_family = base_family, base_size = base_size)

  if (inherits(plot_background, "character") | plot_background == TRUE) {
    ret <- ret + theme(plot.background = element_rect(fill = "#F0F0F0", linetype = 0))
  } else {
    ret <- ret + theme(plot.background = element_rect(fill = NA))
  }

  ret <- ret + theme(legend.background=element_blank())
  ret <- ret + theme(legend.key=element_blank())

  if (inherits(grid, "character") | grid == TRUE) {

    ret <- ret + theme(panel.grid=element_line(color="#2b2b2bdd", size = 0.10))
    ret <- ret + theme(panel.grid.major=element_line(color="#2b2b2b99", size = 0.10))
    ret <- ret + theme(panel.grid.minor=element_line(color="#2b2b2b99", size = 0.05))

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
    ret <- ret + theme(axis.line=element_line(color="#2b2b2b", size=axis_size))
    if (inherits(axis, "character")) {
      axis <- tolower(axis)
      if (regexpr("x", axis)[1] < 0) {
        ret <- ret + theme(axis.line.x=element_blank())
      } else {
        ret <- ret + theme(axis.line.x=element_line(color="#2b2b2b", size=axis_size))
      }
      if (regexpr("y", axis)[1] < 0) {
        ret <- ret + theme(axis.line.y=element_blank())
      } else {
        ret <- ret + theme(axis.line.y=element_line(color="#2b2b2b", size=axis_size))
      }
    } else {
      ret <- ret + theme(axis.line.x=element_line(color="#2b2b2b", size=axis_size))
      ret <- ret + theme(axis.line.y=element_line(color="#2b2b2b", size=axis_size))
    }
  } else {
    ret <- ret + theme(axis.line=element_blank())
  }

  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
    ret <- ret + theme(axis.ticks.x = element_blank())
    ret <- ret + theme(axis.ticks.y = element_blank())
  } else {
    ret <- ret + theme(axis.ticks = element_line(size = 0.15))
    ret <- ret + theme(axis.ticks.x = element_line(size = 0.15))
    ret <- ret + theme(axis.ticks.y = element_line(size = 0.15))
    ret <- ret + theme(axis.ticks.length = grid::unit(5, "pt"))
  }

  xj <- switch(tolower(substr(axis_title_just, 1, 1)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)
  yj <- switch(tolower(substr(axis_title_just, 2, 2)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)

  ret <- ret + theme(axis.text.x=element_text(margin=margin(t=0)))
  ret <- ret + theme(axis.text.y=element_text(margin=margin(r=0)))
  ret <- ret + theme(axis.title=element_text(size=axis_title_size, family=axis_title_family))
  ret <- ret + theme(axis.title.x=element_text(hjust=xj, size=axis_title_size,
                                               family=axis_title_family, face=axis_title_face))
  ret <- ret + theme(axis.title.y=element_text(hjust=yj, size=axis_title_size,
                                               family=axis_title_family, face=axis_title_face))
  ret <- ret + theme(strip.text=element_text(hjust=0, size=strip_text_size,
                                             face=strip_text_face, family=strip_text_family))
  ret <- ret + theme(panel.spacing.x=grid::unit(2, "lines"))
  ret <- ret + theme(panel.spacing.y=grid::unit(2, "lines"))
  ret <- ret + theme(plot.title=element_text(hjust=0, size=plot_title_size,
                                             margin=margin(b=plot_title_margin),
                                             family=plot_title_family, face=plot_title_face))
  ret <- ret + theme(plot.subtitle=element_text(hjust=0, size=subtitle_size,
                                                margin=margin(b=subtitle_margin),
                                                family=subtitle_family, face=subtitle_face))
  ret <- ret + theme(plot.caption=element_text(hjust=1, size=caption_size,
                                               margin=margin(t=caption_margin),
                                               family=caption_family, face=caption_face))
  ret <- ret + theme(plot.margin=plot_margin)

  ret

}

#' Update matching font defaults for text geoms
#'
#' Updates [ggplot2::geom_label] and [ggplot2::geom_text] font defaults
#'
#' @param family,face,size font family name, face and size
#' @export
update_geom_font_defaults <- function(family="Arial Narrow", face="plain", size=3.5) {
  update_geom_defaults("text", list(family=family, face=face, size=size))
  update_geom_defaults("label", list(family=family, face=face, size=size))
}

#' @rdname ArialNarrow
#' @md
#' @title Arial Narrow font name R variable aliases
#' @description `font_an` == "`Arial Narrow`"
#' @format length 1 character vector
#' @export
font_an <- "Arial Narrow"




#' @import ggplot2
NULL
#' @title SciencesPo Publication Theme
#'
#' @description
#' \itemize{
#'  \item \strong{theme_pub()}: Create a publication ready theme
#'  \item \strong{labs_pub()}: Format only plot labels to a publication ready style
#' }
#' @param base_size base font size
#' @param base_family base font family
#' @examples
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'    geom_point(aes(color = gear))
#' p
#'
#' # Use theme_pub()
#' p + theme_pub()
#'
#' # Format labels
#' p + labs_pub()
#'
#' @export
`theme_pub` <-
  function (base_size = 14, base_family = "")
  {
    theme_bw(base_size = base_size, base_family = base_family) %+replace%
      theme(
        panel.border = element_blank(), axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.background = element_rect(colour = "black",
                                        size = 0.5),
        legend.key = element_blank(),
        # Tick labels
        axis.text.x = element_text(size = rel(0.86), colour = "black",face = "bold"),
        axis.text.y = element_text(size = rel(0.86), colour = "black",face = "bold"),

        # Axis
        axis.title = element_text(size = rel(1), colour = "black", face = "bold"),
        axis.line.x = element_line(colour = "black", size = 1),
        axis.line.y = element_line(colour = "black", size = 1),
        axis.ticks = element_line(colour = "black", size = 1),

        # Main title
        plot.title = element_text(size = rel(1), colour = "black" ,
                                  lineheight=1.0, face = "bold"),

        legend.position = "bottom",
        legend.title = element_text(size = rel(0.7), face = "bold", colour = "black"),
        legend.text = element_text(size = rel(0.7), face = "plain", colour = "black")
      )
  }





#' @rdname theme_pub
#' @export
labs_pub <- function(base_size = 14, base_family = ""){
  theme(
    text = element_text(family = base_family,
                        face = "plain", colour = "black", size = base_size,
                        lineheight = 0.9,
                        hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(),
                        debug = FALSE),
    # Tick labels
    axis.text.x = element_text(size = rel(0.86), colour = "black", face = "bold"),
    axis.text.y = element_text(size = rel(0.86), colour = "black", face = "bold"),

    # Axis labels
    axis.title = element_text(size = rel(1), colour = "black", face = "bold"),

    # Main title
    plot.title = element_text(size = rel(1), family = base_family, colour = "black" , lineheight=1.0, face = "bold"),
    legend.title = element_text(size = rel(0.7), face = "bold", colour = "black"),
    legend.text = element_text(size = rel(0.7), face = "plain", colour = "black")
  )
}
NULL



#' A completely blank theme
#'
#' @return theme
#' @export
`theme_base` <- function(){
  theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}
NULL




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
      panel.spacing      = grid::unit(c(0, 0, 0, 0), "cm"),
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





#' ggplot2 theme
#'
#' @description Theme intended to make ggplot2 more readable when used
#' in presentation or papers. Background and major grid lines were dimed
#' and minor grid lines removed to focus the attention on the data.
#'
#' @param base_size base font size
#' @param legend_position position of the legend ("none", "left", "right", "bottom",
#' "top", or two-element numeric vector)
#'
#' @seealso \code{\link{theme_flex}}, \code{\link{theme_pub}}
#' @family themes darkside
#' @keywords ggplot2
#' @import ggplot2
#' @author
#' \Sexpr[stage=build,results=rd]{tools:::Rd_package_author("SciencesPo")}
#' @examples
#' qplot(1:10, (1:10)^2) + theme_plus()
#'
#' # Check that it is a complete theme
#' attr(theme_plus(), "complete")
#' @export
#'
`theme_plus` <- function(base_size = 12, legend_position = 'right'){
  theme(
    # Text size
    text = element_text(size = base_size),
    # Title
    title = element_text(size = rel(1.1), face = 'bold'),
    # Panel title
    strip.text = element_text(face = 'bold'),
    # Panel background color
    strip.background = element_rect(fill = 'grey88'),
    # Axis title
    axis.title = element_text(face = 'bold'),
    # Axis labels
    axis.text = element_text(size = rel(1.1), color = 'black'),
    # Legend
    legend.position = legend_position,
    legend.key = element_blank(),
    legend.background = element_blank(),
    # Background color
    panel.background = element_rect(color = NA, fill = 'grey95'),
    # Plot margin
    plot.margin = grid::unit(c(0.01,0.01,0.01,0.01),'npc'),
    # Minor grid
    panel.grid.minor = element_blank(),
    # Major grid
    panel.grid.major = element_line(color = 'grey88', size = 0.25)
  )
}
NULL
