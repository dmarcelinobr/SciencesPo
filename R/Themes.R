#' @title Theme for mapping
#'
#' @description  Theme for plotting with ggplot.
#'
#' @param base_size Enables to set the font size of all text elements.
#' @param base_family Enables to set the font family of all text elements.
#' @seealso \code{\link[ggplot2]{theme}}, \code{\link{theme_black}}, \code{\link{theme_pub}}.
#' @export
`theme_map` <- function (base_size = 12, base_family = "", legend.position = 'bottom') {
.theme_dumb(base_size = base_size, base_family = base_family) %+replace%
theme(
axis.line=element_blank(),
axis.text.x=element_blank(),
axis.text.y=element_blank(),
axis.ticks=element_blank(),
axis.ticks.length= grid::unit(0.3, "lines"),
#axis.ticks.margin= grid::unit(0.5, "lines"),
axis.title.x=element_blank(),
axis.title.y=element_blank(),
legend.key = element_rect(color = NA),
legend.position = legend.position,
legend.direction = "horizontal",
legend.key.size = grid::unit(0.2, "cm"),
legend.margin = grid::unit(0, "cm"),
legend.title=element_text(size=rel(1.1), face="bold", hjust=0),
panel.background=element_blank(),
panel.border=element_blank(),
panel.grid.major=element_blank(),
panel.grid.minor=element_blank(),
panel.margin= grid::unit(0, "cm"),
plot.background=element_blank(),
# margins starting with top, right, bottom and left
plot.margin= grid::unit(c(.3, .2, 0.3, 0.4), "cm"),
plot.title=element_text(size=rel(1.8), face="bold", hjust=0.5),
strip.background=element_rect(fill="grey90", colour="grey50"),
strip.text.x=element_text(size=rel(0.8)),
strip.text.y=element_text(size=rel(0.8), angle=-90)
)
}


#' @title Themes for ggplot graphs
#'
#' @description  Theme for plotting  with ggplot.
#'
#' @param base_size Enables to set the font size of all text elements.
#' @param base_family Enables to set the font family of all text elements.
#' @param legend.position Enables to set legend position, default is "bottom".
#' @param axis.line.x Enables to set x axis line.
#' @param axis.line.y Enables to set y axis line.
#' @seealso \code{\link[ggplot2]{theme}}, \code{\link{theme_black}}, \code{\link{theme_map}}.
#'
#' @export
`theme_pub` <- function(base_size=14,
                      base_family="helvetica",
                      legend.position = 'bottom',
                      axis.line.x = element_line(),
                      axis.line.y = element_line()){
  (.theme_dumb(base_size=base_size)
  + theme(plot.title = element_text(face = "bold",
size = rel(1.2), hjust = 0.5),
          text = element_text(),
          panel.background = element_rect(color = NA),
          plot.background = element_rect(color = NA),
          panel.border = element_rect(color = NA ),
          axis.title = element_text(face = "bold",size = rel(.9)),
          axis.title.y = element_text(color="#525252",
                                      angle = 90,vjust=1),
          axis.title.x = element_text(color="#525252", vjust = .5),
          axis.text = element_text(color="#525252",size = rel(1)),
          axis.line.x = element_line(),
          axis.line.y = element_line(),
          axis.ticks = element_line(),
axis.ticks.length = grid::unit(0.15, "cm"),
axis.ticks.margin = grid::unit(0, "cm"),
          panel.grid.major = element_line(color="#F0F0F0"),
          panel.grid.minor = element_blank(),
          panel.margin.x = grid::unit(0, "cm"),
          panel.margin.y = grid::unit(0, "cm"),
          legend.key = element_rect(color = NA),
          legend.position = legend.position,
          legend.direction = "horizontal",
          legend.key.size = grid::unit(0.2, "cm"),
          legend.margin = grid::unit(0, "cm"),
          legend.title = element_text(face="italic"),
# margins starting with top, right, bottom and left
          plot.margin = grid::unit(c(0.4, 0.2, 0.1, 0.1),"cm"),
          # strip.background=element_rect(color="#f0f0f0",fill="#f0f0f0"),
          strip.text = element_text(face="bold")
  ))
}
NULL



#' @title Themes for ggplot graphs
#' @description  Theme for plotting  with ggplot.
#' @param base_size Enables to set the font size of all text elements.
#' @param base_family Enables to set the font family of all text elements.
#' @param legend.position Enables to set legend position, default is "none".
#' @export
theme_538 <- function(base_size = 13, base_family = "", legend.position = 'none') {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # Base elements which are not used directly but inherited by others

      line = element_line(colour = '#D0D0D0', size = 0.75,
                          linetype = 1, lineend = "butt"),
      rect = element_rect(fill = "#F0F0F0", colour = "#F0F0F0",
                          size = 0.5, linetype = 1),
      text = element_text(family = base_family, face = 'bold',
                          colour = "#535353", size = base_size,
                          hjust = 0.5, vjust = 0.5, angle = 0,
                          lineheight = 0.9),
      # Modified inheritance structure of text element
      plot.title = element_text(size = rel(1.5), family = '' ,
                                face = 'bold', hjust = -0.05,
                                vjust = 1.5, colour = '#3C3C3C'),
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
      legend.margin = grid::unit(base_size * 1.1, "points"),
      legend.key = element_rect(linetype = 0),
      legend.key.size = grid::unit(1.1, "lines"), legend.key.height = NULL,
      legend.key.width = NULL, legend.text = element_text(size = rel(1.2)),
      legend.text.align = NULL, legend.title = element_text(size = rel(1),
                                                            hjust = 0),
      legend.title.align = NULL,
      legend.position = legend.position,
      legend.direction = NULL, legend.justification = "center",
      #legend.key =  element_rect(colour = '#D0D0D0'),
      # Modifiying legend.position
      complete = TRUE
    )
}
NULL






#' @title Themes for ggplot graphs
#' @description  Theme for plotting  with ggplot.
#'
`theme_fte` <- function (base_size = 13, base_family = "sans", horizontal = TRUE, dkpanel = FALSE){
  ret <- theme(line = element_line(colour = "black", size = 0.5, linetype = 1, lineend = "butt"),
               rect = element_rect(fill = "#D5E4EB", colour = NA, size = 0.5, linetype = 1),
               text = element_text(family = base_family, face = "plain", colour = "black", size = base_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 1), axis.text = element_text(size = rel(1)),
               axis.line = element_line(size = rel(0.8)), axis.line.y = element_blank(),
               axis.text.x = element_text(vjust = 1), axis.text.y = element_text(hjust = 0),
               axis.ticks = element_line(), axis.ticks.y = element_blank(),
               axis.title = element_text(size = rel(1)), axis.title.x = element_text(),
               axis.title.y = element_text(angle = 90), axis.ticks.length = grid::unit(-base_size *
                                                                                   0.5, "points"), axis.ticks.margin = grid::unit(base_size *
                                                                                                                              1.25, "points"),
               legend.background = element_rect(linetype = 0),
               legend.margin = grid::unit(base_size * 1.5, "points"), legend.key = element_rect(linetype = 0),
               legend.key.size = grid::unit(1.2, "lines"), legend.key.height = NULL,
               legend.key.width = NULL, legend.text = element_text(size = rel(1.25)),
               legend.text.align = NULL, legend.title = element_text(size = rel(1),
                                                                     hjust = 0), legend.title.align = NULL, legend.position = "top",
               legend.direction = NULL, legend.justification = "center",
               panel.background = element_rect(linetype = 0), panel.border = element_blank(),
               panel.grid.major = element_line(colour = "#c9c9c9", size = rel(1.5)),
               panel.grid.minor = element_blank(), panel.margin = grid::unit(0.25,
                                                                       "lines"), strip.background = element_rect(fill ="#D5E4EB",
                                                                                                                 colour = NA, linetype = 0), strip.text = element_text(size = rel(1.25)),
               strip.text.x = element_text(), strip.text.y = element_text(angle = -90),
               plot.background = element_rect(fill = "#D5E4EB",
                                              colour = NA), plot.title = element_text(size = rel(1.5), hjust = 0, face = "bold"), plot.margin = grid::unit(c(6, 5, 6, 5) * 2, "points"), complete = TRUE)
  if (horizontal) {
    ret <- ret + theme(panel.grid.major.x = element_blank())
  }
  else {
    ret <- ret + theme(panel.grid.major.y = element_blank())
  }
  if (dkpanel == TRUE) {
    ret <- ret + theme(panel.background = element_rect(fill = "#c3d6df"),
                       strip.background = element_rect(fill = "#c3d6df"))
  }
  ret
}
NULL

#' @title ggplot2 Theme with No Background or Gridlines.
#'
#' @description A ggplot2 theme with no background and no gridlines.
#'
#' @param base_size The size to use for text.  Various textual components are
#' scaled off of this value.
#' @param base_family The base font family.
#' @export
#' @seealso \code{\link[ggplot2]{theme}}, \code{\link{theme_pub}}, \code{\link{theme_map}}.

theme_black<-function(base_size=14, base_family ="helvetica") {
  .theme_dumb(base_size=base_size) +
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
      # Specify axis options
      axis.line=element_blank(),
      axis.text.x=element_text(size=base_size*0.8,color="white",
                               lineheight=0.9,vjust=1),
      axis.text.y=element_text(size=base_size*0.8,color="white",
                               lineheight=0.9,hjust=1),
      axis.ticks=element_line(color="white",size = 0.2),
      axis.title.x=element_text(size=base_size,color="white",vjust=1),
      axis.title.y=element_text(size=base_size,color="white",angle=90,
                                vjust=0.5),
      axis.ticks.length=grid::unit(0.3,"lines"),
      axis.ticks.margin=grid::unit(0.5,"lines"),
      # Specify legend options
      legend.background=element_rect(color=NA,fill="black"),
      legend.key=element_rect(color="white", fill="black"),
      legend.key.size=grid::unit(1.2,"lines"),
      legend.key.height=NULL,
      legend.key.width=NULL,
      legend.text=element_text(size=base_size*0.8,color="white"),
      legend.title=element_text(size=base_size*0.8,face="bold",hjust=0,
                                color="white"),
      legend.position="right",
      legend.text.align=NULL,
      legend.title.align=NULL,
      legend.direction="vertical",
      legend.box=NULL,
      # Specify panel options
      panel.background=element_rect(fill="black",color = NA),
      panel.border=element_rect(fill=NA,color="white"),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      panel.margin=grid::unit(0.25,"lines"),
      # Specify facetting options
      strip.background=element_rect(fill="grey30",color="grey10"),
      strip.text.x=element_text(size=base_size*0.8,color="white"),
      strip.text.y=element_text(size=base_size*0.8,color="white", angle=-90),
      # Specify plot options
      plot.background=element_rect(color="black",fill="black"),
      plot.title=element_text(size=base_size*1.2,color="white"),
      plot.margin = grid::unit(c(0.4, 0.2, 0.1, 0.1),"cm")
    )
}



#' @title Scale colors for ggplot graphs
#' @description Discrete scale for the theme_pub().
#' @param \dots parameters to be used.
#' @export
#' @rdname scale_fill_pub
scale_fill_pub <- function(...){
  discrete_scale("fill",
                          "pub",
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
#' @rdname scale_colour_pub
scale_colour_pub <- function(...){
discrete_scale("colour",
                        "pub",
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

# http://color.adobe.com/

# "#9b59b6", "#3498db", "#95a5a6", "#e74c3c", "#34495e", "#2ecc71" "FF7E0D"

# "windows blue", "amber", "greyish", "faded green", "dusty purple"





#' @title Themes for ggplot graphs
#'
#' @description A dumb theme.
#' @param base_size Enables to set the font size of all text elements.
#' @param base_family Enables to set the font family of all text elements.
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
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    axis.title = element_text(),
    axis.title.x = element_text(),
    axis.title.y = element_text(),
    axis.text = element_text(),
    axis.text.x = element_text(),
    axis.text.y = element_text(),
    axis.ticks = element_line(),
    axis.ticks.x = element_line(),
    axis.ticks.y = element_line(),
    axis.ticks.length = grid::unit(0.15, "cm"),
    #axis.ticks.margin =grid::unit(0.1, "cm"),
    axis.line = element_line(),
    axis.line.x = element_line(),
    axis.line.y = element_line(),
    legend.background = element_rect(colour = NA),
    legend.margin = grid::unit(0.2, "cm"),
    legend.key = element_rect(),
    legend.key.size = grid::unit(1.2, "lines"),
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
    panel.margin = grid::unit(0, "cm"),
    panel.grid = element_line(),
    panel.grid.major = element_line(),
    panel.grid.major.x = element_line(),
    panel.grid.major.y = element_line(),
    panel.grid.minor = element_line(),
    panel.grid.minor.x = element_line(),
    panel.grid.minor.y = element_line(),
    plot.background = element_rect(),
    plot.title = element_text(),
    plot.margin = grid::unit(c(0.3, 0.2, 0.3, 0.4), "cm"),
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
    axis.text.x = element_text(vjust = 0.1),
    axis.title.y = element_text(vjust=0.1),
    legend.text = element_text(size = rel(0.8)),
    legend.title = element_text(size = rel(0.8), hjust = 0),
    panel.grid.minor = element_line(size = 0.25),
    plot.title = element_text(size = rel(1.2)),
    strip.text.y = element_text(angle = -90)
  )
}


#' @title Dumb Theme
#'
#' @description This theme is designed to be a dumb or foundation from which to build new
#' themes. It is a complete theme with only minimal number of elements defined.
#'
#' @inheritParams theme_grey
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
NULL
