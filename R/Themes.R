#' Themes for ggplot graphs
#'
#' Theme for mapping  with ggplot
#' @param base_size Enables to set the font size of all text elements.
#' @param base_family Enables to set the font family of all text elements.
#'
#' @importFrom ggplot2 rel element_rect element_line element_text element_blank theme theme_bw %+replace%
#' @importFrom grid unit
#'
#' @export
theme_pub <- function(base_size=14, base_family="helvetica") {
  # library(grid)
  (.theme_dumb(base_size=base_size)
  + theme(plot.title = element_text(face = "bold",
size = rel(1.2), hjust = 0.5),
          text = element_text(),
          panel.background = element_rect(colour = NA),
          plot.background = element_rect(colour = NA),
          panel.border = element_rect(colour = NA),
          axis.title = element_text(face = "bold",size = rel(1)),
          axis.title.y = element_text(angle=90,vjust =2),
          axis.title.x = element_text(vjust = -0.2),
          axis.text = element_text(),
          axis.line = element_line(colour="black"),
          axis.ticks = element_line(),
          panel.grid.major = element_line(colour="#f0f0f0"),
          panel.grid.minor = element_blank(),
          legend.key = element_rect(colour = NA),
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.key.size= unit(0.2, "cm"),
          legend.margin = unit(0, "cm"),
          legend.title = element_text(face="italic"),
          plot.margin=unit(c(10,5,5,5),"mm"),
          strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
          strip.text = element_text(face="bold")
  ))
}
NULL




#' @title Scale colors for ggplot graphs
#' @param \dots parameters to be used.
#' @export
#' @rdname scale_fill_pub
scale_fill_pub <- function(...){
  ggplot2::discrete_scale("fill","pub",scales::manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33", "#FE7C96")), ...)
}
NULL




#' @title Scale colors for ggplot graphs
#' @param \dots parameters to be used.
#' @export
#' @rdname scale_colour_pub
scale_colour_pub <- function(...){
ggplot2::discrete_scale("colour","pub",scales::manual_pal(values = c("#386cb0","#FF9146","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33","#4574c9")), ...)
}
NULL




#' Themes for ggplot graphs
#'
#' Theme for mapping  with ggplot
#' @param base_size Enables to set the font size of all text elements.
#' @param base_family Enables to set the font family of all text elements.
#'
#' @importFrom ggplot2 element_rect element_line element_text element_blank theme theme_bw %+replace%
#' @importFrom grid unit
#'
#' @export
.theme_dumb_ <- function(base_size=12, base_family="") {
  theme(
    line = element_line(colour = "black", size = 0.5,
                        linetype = 1, lineend = "butt"),
    rect = element_rect(fill = "white", colour = "black",
                        size = 0.5, linetype = 1),
    text =  element_text(family = base_family, face = "plain",
                         colour = "black", size = base_size,
                         hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9),
    axis.text = element_text(),
    strip.text = element_text(),
    axis.line = element_blank(),
    axis.title= element_text(),
    axis.title.x = element_text(),
    axis.title.y = element_text(),
    axis.text = element_text(),
    axis.text.x = element_text(),
    axis.text.y = element_text(),
    axis.ticks = element_line(),
    axis.ticks.x = element_line(),
    axis.ticks.y = element_line(),
    axis.ticks.length =  unit(0.15, "cm"),
    axis.ticks.margin =  unit(0.1, "cm"),
    axis.line = element_line(),
    axis.line.x = element_line(),
    axis.line.y = element_line(),
    legend.background = element_rect(colour = NA),
    legend.margin = unit(0.2, "cm"),
    legend.key = element_rect(),
    legend.key.size = unit(1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = element_text(),
    legend.text.align = NULL,
    legend.title = element_text(),
    legend.title.align = NULL,
    legend.position = "right",
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,
    ## Must have colour=NA or covers the plot
    panel.background = element_rect(),
    panel.border = element_rect(fill=NA),
    panel.margin = unit(0.25, "lines"),
    panel.grid = element_line(),
    panel.grid.major = element_line(),
    panel.grid.major.x = element_line(),
    panel.grid.major.y = element_line(),
    panel.grid.minor = element_line(),
    panel.grid.minor.x = element_line(),
    panel.grid.minor.y = element_line(),
    plot.background = element_rect(),
    plot.title = element_text(),
    plot.margin = unit(c(1, 1, 0.5, 0.5), "lines"),
    strip.background = element_rect(),
    strip.text = element_text(),
    strip.text.x = element_text(),
    strip.text.y = element_text(angle = -90),
    complete = TRUE)
}


.theme_dumb_sizes <- function() {
  theme(
    axis.text.y = element_text(hjust = 1),
    strip.text = element_text(size = rel(0.8)),
    axis.text = element_text(size = rel(0.8)),
    axis.text.x = element_text(vjust = 1),
    legend.text = element_text(size = rel(0.8)),
    legend.title = element_text(size = rel(0.8), hjust = 0),
    panel.grid.minor = element_line(size = 0.25),
    plot.title = element_text(size = rel(1.2)),
    strip.text.y = element_text(angle = -90)
  )
}


#' Dumb Theme
#'
#' This theme is designed to be a dumb or foundation from which to build new
#' themes. It is a complete theme with only minimal number of elements defined.
#'
#' @inheritParams ggplot2::theme_grey
#' @param use_sizes If \code{TRUE}, then define sizes and locations
#' with reasonable defaults taken from \code{\link{theme_gray}}.
#'
#' @family themes
#' @export
.theme_dumb <- function(base_size=12, base_family="", use_sizes=TRUE) {
  thm <- .theme_dumb_(base_size=base_size, base_family=base_family)
  if (use_sizes) {
    thm + .theme_dumb_sizes()
  }
  thm
}

