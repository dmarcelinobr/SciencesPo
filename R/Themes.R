#' @encoding UTF-8
#' @title The Default Theme
#'
#' @description After loading the SciencesPo package, this theme will be
#' set to default for all subsequent graphs made with ggplot2.
#'
#' @param legend Enables to set legend position, default is "bottom".
#' @param font_family Default font family.
#' @param font_size Overall font size. Default is 14.
#' @param line_width Default line size.
#' @param axis.line.x Enables to set x axis line.
#' @param axis.line.y Enables to set y axis line.
#' @return The theme.
#' @seealso \code{\link[ggplot2]{theme}}, \code{\link{theme_538}}, \code{\link{theme_blank}}.
#' @examples
#' ggplot(diamonds,aes(cut, group=1)) + geom_bar()+
#' geom_freqpoly(stat="count",size=2) + scale_color_pub() + theme_pub(line_width=1)
#'
#' dat <- data.frame()
#' for(i in 1:4)
#' dat <- rbind(dat, data.frame(set=i, x=anscombe[,i], y=anscombe[,i+4]))
#'
#' ggplot(dat, aes(x, y)) + geom_point(size=5, color="red",
#' fill="orange", shape=21) + geom_smooth(method="lm", fill=NA,
#' fullrange=TRUE) + facet_wrap(~set, ncol=2)
#'
#' @export
`theme_pub` <- function(legend = 'bottom',
                        font_family = 'sans',
                        font_size = 13,
                        line_width = .5,
                        axis.line.x = element_line(),
                        axis.line.y = element_blank()){
half_line <- font_size / 2
small_rel <- 0.857
small_size <- small_rel * font_size
theme_grey(base_size = font_size, base_family = font_family) %+replace%
theme(rect = element_rect(fill = "transparent",
                              colour = NA,
                              color = NA,
                              size = 0,
                              linetype = 0),
          text = element_text(family = font_family,
                              face = "plain",
                              colour = "black",
                              size = font_size,
                              hjust = 0.5,
                              vjust = 0.5,
                              angle = 0,
                              lineheight = .9,
                              margin = ggplot2::margin(),
                              debug = FALSE),
axis.text = element_text(color="black", size = small_size),
          axis.title = element_text(face = "bold"),
          axis.text.x = element_text(margin = ggplot2::margin(t = small_size / 4), vjust = 1),
          axis.text.y = element_text(margin = ggplot2::margin(r = small_size / 4), hjust = 1),
          axis.title.x = element_text(
            margin = ggplot2::margin(t = small_size / 2, b = small_size / 4)
          ),
          axis.title.y = element_text(
            angle = 90,
            margin = ggplot2::margin(r = small_size / 2, l = small_size / 4),
          ),
axis.ticks = element_line(color = "#525252", size = line_width),
axis.line = element_line(color = "#525252", size = line_width),
          legend.position = legend,
          # legend.position = c(-0.03, 1.05),
          # legend.justification = c("left", "top"),
          legend.direction = "horizontal",
          legend.key = element_rect(color = NA),
          legend.margin = grid::unit(0, "cm"),
          legend.key.size = grid::unit(0.2, "cm"),
          legend.title = element_text(face="italic"),
          legend.text  = element_text(size = rel(small_rel)),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_line(color="#F0F0F0"),
          panel.grid.minor = element_blank(),
          strip.text = element_text(face="bold", size = rel(small_rel)),
strip.background = element_rect(fill = "grey80", color = "grey50", size = 0),
# margins starting with top, right, bottom and left
      plot.margin = grid::unit(c(0.4, 0.2, 0.1, 0.1),"cm"),
          plot.background = element_blank(),
          plot.title = element_text(face = "bold",
                                    size = font_size,
                                    margin = ggplot2::margin(b = half_line))
    )
}
NULL





#' @title Themes for ggplot2 Graphs
#'
#' @description  Theme for plotting  with ggplot2.
#'
#' @param legend Enables to set legend position, default is "none".
#' @param font_family Default font family.
#' @param font_size Overall font size. Default is 13.
#' @return The theme.
#'
#' @examples
#' qplot(1:10, (1:10)^3) + theme_fte()
#'
#' @export
#' @aliases theme_538
`theme_fte` <- function(legend = 'none', font_size = 12, font_family = 'sans'){
theme_grey(base_size = font_size, base_family = font_family) %+replace%
    theme(
      # Base elements which are not used directly but inherited by others

      line = element_line(color = '#D0D0D0', size = 0.75,
                          linetype = 1, lineend = "butt"),
      rect = element_rect(fill = "#F0F0F0", color = "#F0F0F0",
                          size = 0.5, linetype = 1),
      text = element_text(family = font_family, face = 'bold',
                          color = "#535353", size = font_size,
                          hjust = 0.5, vjust = 0.5, angle = 0,
                          lineheight = 0.9, margin = ggplot2::margin(), debug = FALSE),
      # Modified inheritance structure of text element
plot.title = element_text(size = rel(1.5), family = '' ,
                                face = 'bold', hjust = -0.05,
                                vjust = 1.5, color = '#3C3C3C', margin = ggplot2::margin(), debug = FALSE),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text = element_text(),
      # Modified inheritance structure of line element
      axis.ticks =  element_line(),
      panel.grid.major =  element_line(),
      panel.grid.minor =  element_blank(),
      # Modified inheritance structure of rect element
      plot.background =  element_rect(),
      panel.background =  element_rect(),
     strip.background = element_rect(),
      legend.background = element_rect(linetype = 0),
      legend.margin = grid::unit(font_size * 1.1, "points"),
      legend.key = element_rect(linetype = 0),
      legend.key.size = grid::unit(1.1, "lines"), legend.key.height = NULL,
      legend.key.width = NULL, legend.text = element_text(size = rel(1.2)),
      legend.text.align = NULL, legend.title = element_text(size = rel(1),
                                                            hjust = 0),
      legend.title.align = NULL,
      legend.position = legend,
      legend.direction = NULL,
      legend.box = "vertical",
      legend.justification = "center",
      #legend.key =  element_rect(color = '#D0D0D0'),
      # Modifiying legend.position
      complete = TRUE
    )
}
NULL

#' @export
#' @rdname theme_fte
theme_538 <- theme_fte


#' Create a Completely Empty Theme
#'
#' The theme created by this function shows nothing but the plot panel.
#' @param font_family Default font family.
#' @param font_size Overall font size. Default is 12.
#' @return The theme.
#' @examples
#' # plot with small amount of remaining padding
#' qplot(1:10, (1:10)^2) + theme_blank()
#' # remaining padding removed
#' qplot(1:10, (1:10)^2) + theme_blank() + labs(x = NULL, y = NULL)
#' @export
`theme_blank` <- function(font_size = 12, font_family = ""){
  theme_grey(base_size = font_size, base_family = font_family) %+replace%
    theme(
      rect              = element_rect(fill = "transparent", colour = NA, color = NA, size = 0, linetype = 0),
      line              = element_blank(),
      text              = element_blank(),
      title             = element_blank(),
      # to debug, uncomment next line
      #plot.background   = element_rect(colour = "blue", fill = "cyan"),
      panel.background  = element_blank(),
      axis.ticks.length = grid::unit(0, "cm"),
      legend.position   = "none",
      panel.margin      = grid::unit(c(0, 0, 0, 0), "cm"),
      plot.margin       = grid::unit(c(0, 0, 0, 0), "cm")
    )
}
NULL





#' @title Add/Modify/Remove the background grid in a ggplot2 plot
#'
#' This function provides a simple way to modify the background grid in ggplot2. It
#' doesn't do anything that can't be done just the same with \code{theme()}. However, it simplifies
#' creation of the most commonly needed variations.
#' @param major Specifies along which axes you would like to plot major grid lines. Options are "xy", "x",
#'  "y", "only_minor" (disables major grid lines but allows you to switch on minor grid lines), "none".
#' @param minor Specifies along which axes you would like to plot minor grid lines. Options are "xy", "x",
#'  "y", "none".
#' @param size.major Size of the major grid lines.
#' @param size.minor Size of the minor grid lines.
#' @param colour.major Color of the major grid lines.
#' @param colour.minor Color of the minor grid lines.
#' @export
`background_grid` <- function(major = c("xy", "x", "y", "only_minor", "none"),
                            minor = c("xy", "x", "y", "none"),
                            size.major = 0.2, size.minor = 0.5,
                            colour.major = "grey90", colour.minor = "grey98"){

  if (major[1] == "none") return(theme(panel.grid = element_blank()))

  t <- switch( major[1],
               x = theme(panel.grid.major   = element_line(colour = colour.major,
                                                           size = size.major),
                         panel.grid.major.y = element_blank()),
               y = theme(panel.grid.major   = element_line(colour = colour.major,
                                                           size = size.major),panel.grid.major.x = element_blank()), xy = theme(panel.grid.major = element_line(colour = colour.major, size = size.major)),
yx = theme(panel.grid.major = element_line(colour = colour.major,
size = size.major)),
theme(panel.grid.major = element_blank()))
  t + switch( minor[1],
x = theme(panel.grid.minor   = element_line(colour = colour.minor,
size = size.minor),
panel.grid.minor.y = element_blank()),
y = theme(panel.grid.minor   = element_line(colour = colour.minor,
size = size.minor),
panel.grid.minor.x = element_blank()),
xy = theme(panel.grid.minor = element_line(colour = colour.minor,
size = size.minor)),
yx = theme(panel.grid.minor = element_line(colour = colour.minor,
size = size.minor)),
theme(panel.grid.minor = element_blank()))
}
NULL



#' @title Scale color-blind-friendly for ggplot graphs
#' @description Discrete  color-blind-friendly scale for ggplot().
#' @param \dots parameters to be used.
#' @export
#' @rdname scale_color_blind
#' @aliases scale_colour_blind
scale_color_blind <- function(...){
  discrete_scale("color", "blind",
                 scales::manual_pal(values =
                                      c("#000000",
                                        "#E69F00",
                                        "#56B4E9",
                                        "#009E73",
                                        "#F0E442",
                                        "#0072B2",
                                        "#D55E00",
                                        "#CC79A7",
                                        "#999999")
                 ), ...)
}
NULL




#' @title Palette data for the themes used by package
#'
#' @description Data used by the palettes in the package.
#'
#' @format A \code{list}.
#'
themes_data <- {
x <- list()
x$pub <- list()
x$pub$colors <-
  list(tableau20 =
  c(rgb(31, 119, 180, max = 255),
    rgb(174, 199, 232, max = 255),
    rgb(255, 127, 14, max = 255),
    rgb(255, 187, 120, max = 255),
    rgb(44, 160, 44, max = 255),
    rgb(152, 223, 138, max = 255),
    rgb(214, 39, 40, max = 255),
    rgb(255, 152, 150, max = 255),
    rgb(148, 103, 189, max = 255),
    rgb(197, 176, 213, max = 255),
    rgb(140, 86, 75, max = 255),
    rgb(196, 156, 148, max = 255),
    rgb(227, 119, 194, max = 255),
    rgb(247, 182, 210, max = 255),
    rgb(127, 127, 127, max = 255),
    rgb(199, 199, 199, max = 255),
    rgb(188, 189, 34, max = 255),
    rgb(219, 219, 141, max = 255),
    rgb(23, 190, 207, max = 255),
    rgb(158, 218, 229, max = 255))
  )

x$parties <- list()

x$parties$BRA <- c(PT=rgb(255,39,0, max = 255),
                     PMDB=rgb(255,153,0, max = 255),
                     PSDB=rgb(0,143,213, max = 255),
                     PSB=rgb(213,94,0, max = 255),
                     PV=rgb(119,171,67, max = 255))


x$fte<-c(red=rgb(255,39,0, max = 255),
         blue=rgb(0,143,213, max = 255),
         green=rgb(119,171,67, max = 255),
         orange=rgb(230,159,0, max = 255))

x$seasons <-c(autumn=rgb(16,78,139, max = 255),
              spring=rgb(110,139,61, max = 255),
              summer=rgb(154,50,205, max = 255),
              winter=rgb(255,193,37, max = 255))

x$colorblind <- c(rgb(0,107,164, max = 255),
                  rgb(255,128,14, max = 255),
                  rgb(171,171,171, max = 255),
                  rgb(89,89,89, max = 255),
                  rgb(95,158,209, max = 255),
                  rgb(200,82,0, max = 255),
                  rgb(137,137,137, max = 255),
                  rgb(162,200,236, max = 255),
                  rgb(255,188,121, max = 255),
                  rgb(207,207,207, max = 255))

## return
x
}
NULL


#' @title Color Palettes for Publication (discrete)
#'
#' @description Color palettes based on \href{http://www.tableausoftware.com/}{Tableau}.
#'
#' @param palette Palette name.
#' @examples
#' library(scales)
#' show_col(pub_color_pal("tableau20")(20))
#' @export
#'
`pub_color_pal` <- function(palette = "tableau10") {
  pal.list <- themes_data$pub$colors
  if (!palette %in% c(names(pal.list), "tableau20", "tableau10", "tableau10light")) {
    stop(sprintf("%s is not a valid palette name", palette))
  }
  if (palette == "tableau10") {
    types <- pal.list[["tableau20"]][seq(1, 20, by = 2)]
  } else if (palette == "tableau10light") {
    types <- pal.list[["tableau20"]][seq(2, 20, by = 2)]
  } else {
    types <- pal.list[[palette]]
  }
  function(n) {
    unname(types)[seq_len(n)]
  }
}
NULL


#' @title Publication color scales.
#'
#' @description See \code{\link{pub_color_pal}} for details.
#'
#' @inheritParams ggplot2::scale_colour_hue
#' @inheritParams pub_color_pal
#' @family colour publication
#' @rdname scale_color_pub
#' @export
#' @seealso \code{\link{pub_color_pal}} for references.
#'
scale_colour_pub <- function(palette = "tableau10", ...) {
  discrete_scale("colour", "tableau", pub_color_pal(palette), ...)
}
NULL

#' @export
#' @rdname scale_color_pub
scale_fill_pub <- function(palette = "tableau10", ...) {
  discrete_scale("fill", "pub", pub_color_pal(palette), ...)
}
NULL



#' @export
#' @rdname scale_color_pub
scale_color_pub <- scale_colour_pub




#' Extended fivethirtyeight.com color palette
#'
#' The standard fivethirtyeight.com palette for line plots is blue, red, green.
#'  I add an orange ton.
#'
#' @family colour fte
#' @examples
#' scales::show_col(fte_color_pal()(4))
#' @export
fte_color_pal <- function() {
  function(n) {
    colors <- themes_data$fte[c("blue", "red", "green", "orange")]
    unname(colors[seq_len(n)])
  }
}
NULL



#' fivethirtyeight.com color scales
#'
#' Color scales using the colors in the fivethirtyeight graphics.
#'
#' @inheritParams ggplot2::scale_colour_hue
#' @family colour fte
#' @rdname scale_fte
#' @seealso \code{\link{theme_538}} for examples.
#' @export
scale_colour_fte <- function(...) {
 discrete_scale("colour", "538", fte_color_pal(), ...)
}
NULL



#' @rdname scale_fte
#' @export
scale_color_fte <- scale_colour_fte

#' @rdname scale_fte
#' @export
scale_fill_fte <- function(...) {
  discrete_scale("fill", "538", fte_color_pal(), ...)
}
NULL


#' Color blind color palette (discrete)
#'
#' Color palettes for color blind.
#'
#' @family colour colorblind
#' @examples
#' scales::show_col(colorblind_pal()(8))
#' @export
colorblind_pal <- function() {
  scales::manual_pal(unname(themes_data$colorblind))
}
NULL


#' Color blind color palette (discrete)
#'
#' Color palettes for color blind.
#'
#' @inheritParams ggplot2::scale_colour_hue
#' @family colour colorblind
#' @rdname scale_colorblind
#' @export
scale_fill_colorblind <- function(...) {
  discrete_scale("fill", "colorblind", colorblind_pal(), ...)
}
NULL



#' @export
#' @rdname scale_colorblind
scale_colour_colorblind <- function(...) {
  discrete_scale("colour", "colorblind", colorblind_pal(), ...)
}
NULL


#' @export
#' @rdname scale_colorblind
scale_color_colorblind <- scale_colour_colorblind






#' @title Color Palettes for Political Organizations (discrete)
#'
#' @description Color palettes for political organizations.
#'
#' @param palette Palette name.
#' @family colour parties
#' @examples
#' scales::show_col(parties_color_pal()(10))
#' @export
#'
`parties_color_pal` <- function(palette = "BRA") {
  pal.list <- themes_data$parties
  if (!palette %in% c(names(pal.list), "BRA", "ARG", "CAN", "USA")) {
    stop(sprintf("%s is not a valid palette name", palette))
  }
  if (palette == "BRA") {
    types <- pal.list[["BRA"]][seq(1, 20, by = 1)]
  } else if (palette == "ARG") {
    types <- pal.list[["ARG"]][seq(1, 20, by = 1)]
  } else {
    types <- pal.list[[palette]]
  }
  function(n) {
    unname(types)[seq_len(n)]
  }
}
NULL


#' @title Political Parties Color Scales
#'
#' @description Scale color for political parties.
#'
#' @inheritParams ggplot2::scale_colour_hue
#' @inheritParams parties_color_pal
#' @family colour parties
#' @rdname scale_color_parties
#' @export
#' @seealso \code{\link{parties_color_pal}} for references.
#'
scale_colour_parties <- function(palette = "BRA", ...) {
  discrete_scale("colour", "parties", parties_color_pal(palette), ...)
}
NULL


#' @export
#' @rdname scale_color_parties
scale_fill_parties <- function(palette = "BRA", ...) {
  discrete_scale("fill", "parties", parties_color_pal(palette), ...)
}
NULL



#' @export
#' @rdname scale_color_parties
scale_color_parties <- scale_colour_parties


# http://color.adobe.com/
# http://www.farb-tabelle.de/en/rgb2hex.htm?
# "#9b59b6", "#3498db", "#95a5a6", "#e74c3c", "#34495e", "#2ecc71" "FF7E0D"
