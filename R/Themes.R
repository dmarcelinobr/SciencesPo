#' @encoding UTF-8
#' @title Makes the Default Theme
#'
#' @description After loading the SciencesPo package, this theme will be
#' set to default for all graphs made with ggplot2.
#'
#' @param font_family Default font family.
#' @param font_size Overall font size. Default is 14.
#' @param line_size Default line size.
#' @param legend Enables to set legend position, default is "bottom".
#' @param axis.line.x Enables to set x axis line.
#' @param axis.line.y Enables to set y axis line.
#' @return The theme.
#' @seealso \code{\link[ggplot2]{theme}}, \code{\link{theme_538}}, \code{\link{theme_blank}}.
#' @examples
#' ggplot(diamonds,aes(cut, group=1)) + geom_bar()+
#' geom_freqpoly(stat="count",size=2) + scale_color_pub() + theme_pub(line_size=1)
#'
#' dat <- data.frame()
#' for(i in 1:4)
#' dat <- rbind(dat, data.frame(set=i, x=anscombe[,i], y=anscombe[,i+4]))
#'
#' ggplot(dat, aes(x, y)) + geom_point(size=5, color="red",
#' fill="orange", shape=21) + geom_smooth(method="lm", fill=NA,
#' fullrange=TRUE) + facet_wrap(~set, ncol=2) + scale_color_pub()
#'
#' @export
`theme_pub` <- function(font_family = '',
                        font_size = 14,
                        line_size = .5,
                        legend = 'bottom',
                        axis.line.x = element_line(),
                        axis.line.y = element_line()){
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
axis.ticks = element_line(color = "#525252", size = line_size),
axis.line = element_line(color = "#525252", size = line_size),
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
#' @param font_family Default font family.
#' @param font_size Overall font size. Default is 13.
#' @param legend Enables to set legend position, default is "none".
#' @return The theme.
#'
#' @examples
#' qplot(1:10, (1:10)^3) + theme_538()
#' @export
theme_538 <- function(font_size = 13, font_family = '', legend = 'none'){
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
      legend.justification = "center",
      #legend.key =  element_rect(color = '#D0D0D0'),
      # Modifiying legend.position
      complete = TRUE
    )
}
NULL




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






#' @title Scale colors for ggplot graphs
#' @description Discrete scale for the theme_pub().
#' @param \dots parameters to be used.
#' @export
#' @rdname scale_fill_pub
scale_fill_pub <- function(...){
  discrete_scale("fill", "pub",
                          scales::manual_pal(values =
                                               c("#386CB0",
                                                 "#FF6347",
                                                 "#2ECC71",
                                                 "#FDB462",
                                                 "#9B59B6",
                                                 "#899DA4",
                                                 "#7FC97F",
                                                 "#662506",
                                                 "#3C2520",
                                                 "#A6CEE3",
                                                 "#FB9A99",
                                                 "#984EA3",
                                                 "#FFFF33",
                                                 "#FE7C96",
                                                 "#CA9743")
                                             ), ...)
}
NULL




#' @title Scale colors for ggplot graphs
#' @description Discrete scale colors for the theme_pub().
#' @param \dots parameters to be used.
#' @export
#' @rdname scale_color_pub
scale_color_pub <- function(...){
discrete_scale("color", "pub",
                        scales::manual_pal(values =
                                             c("#386CB0",
                                               "#FF6347",
                                               "#2ECC71",
                                               "#FDB462",
                                               "#9B59B6",
                                               "#899DA4",
                                               "#7FC97F",
                                               "#662506",
                                               "#3C2520",
                                               "#A6CEE3",
                                               "#FB9A99",
                                               "#984EA3",
                                               "#FFFF33",
                                               "#FE7C96",
                                               "#CA9743")
                                           ), ...)
}
NULL



#' @title Scale color-blind-friendly for ggplot graphs
#' @description Discrete  color-blind-friendly scale for ggplot().
#' @param \dots parameters to be used.
#' @export
#' @rdname scale_color_blind
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


#' @title The Tableau 20 colors for ggplot graphs
#' @description Discrete scale colors like the Tableau 20 colors.
#' @param \dots parameters to be used.
#' @export
#' @rdname scale_fill_tableau20
scale_fill_tableau20 <- function(...){
  discrete_scale("fill", "tableau",
                 scales::manual_pal(values = c("#1F77B4", "#AEC7E8", "#FF7F0E", "#FFBB78", "#2CA02C", "#98DF8A", "#D62728", "#FF9896", "#9467BD", "#C5B0D5", "#8C564B", "#C49C94", "#E377C2", "#F7B6D2", "#7F7F7F", "#C7C7C7", "#BCBD22", "#DBDB8D", "#17BECF", "#9EDAE5") ), ...)
}
NULL


#' @title Scale color for ggplot graphs
#' @description Discrete  color scale for ggplot2.
#' @param \dots parameters to be used.
#' @export
#' @rdname scale_fill_tableau20
scale_color_tableau20 <- function(...){
  discrete_scale("color", "tableau",
                 scales::manual_pal(values = c("#1F77B4", "#AEC7E8", "#FF7F0E", "#FFBB78", "#2CA02C", "#98DF8A", "#D62728", "#FF9896", "#9467BD", "#C5B0D5", "#8C564B", "#C49C94", "#E377C2", "#F7B6D2", "#7F7F7F", "#C7C7C7", "#BCBD22", "#DBDB8D", "#17BECF", "#9EDAE5") ), ...)
}
NULL

# http://color.adobe.com/

# "#9b59b6", "#3498db", "#95a5a6", "#e74c3c", "#34495e", "#2ecc71" "FF7E0D"

# "windows blue", "amber", "greyish", "faded green", "dusty purple"
