#' @include SciencesPo.R
#' @include Palettes.R
NULL

#' @encoding UTF-8
#' @title Preview ggplot2 themes
#' @description Used to preview ggplot2 themes.
#' @export
PreviewTheme <- function () {
  x_values <- c('A', 'B', 'C')
  blank_data <- data.frame(
    x = factor(x_values, x_values),
    y = seq(10, 30, 10),
    check.names = FALSE
  )
  ggplot(blank_data, aes_string(x = 'x', y = 'y')) +
    geom_blank() +
    scale_y_continuous(
      label = function(x)
        format(x, scientific = FALSE, trim = TRUE)
    ) +
    labs(x = 'x-axis title', y = 'y-axis title', title = 'Plot Title')
}


#' @encoding UTF-8
#' @title The Default Theme
#'
#' @description After loading the SciencesPo package, this theme will be
#' set to default for all subsequent graphs made with ggplot2.
#'
#' @param legend Enables to set legend position, default is "bottom".
#' @param base_family Default font family.
#' @param base_size Overall font size. Default is 14.
#' @param horizontal Logical. Horizontal axis lines?
#' @param line_width Default line size.
#' @param axis_line Enables to set x and y axes.
#' @family themes pub
#' @return The theme.
#' @seealso \code{\link[ggplot2]{theme}}, \code{\link{theme_538}}, \code{\link{theme_blank}}.
#' @examples
#' PreviewTheme()+theme_pub()
#'
#' ggplot(diamonds,aes(cut, group=1)) + geom_bar()+
#' geom_freqpoly(stat="count",size=2) + theme_pub(line_width=2.5)
#'
#' ggplot(diamonds,aes(cut, group=1)) +
#' geom_bar()+
#' geom_freqpoly(stat="count",size=2) +
#' theme_pub(base_family='serif')
#'
#' dat <- data.frame()
#' for(i in 1:4)
#' dat <- rbind(dat, data.frame(set=i, x=anscombe[,i], y=anscombe[,i+4]))
#'
#' ggplot(dat, aes(x, y)) + geom_point(size=5, color="red",
#' fill="orange", shape=21) +
#' geom_smooth(method="lm", fill=NA, fullrange=TRUE) +
#' facet_wrap(~set, ncol=2) +
#' theme_pub(base_family='serif')
#'
#'
#' ggplot(dat, aes(x, y)) + geom_point(size=5, color="red",
#' fill="orange", shape=21) +
#' geom_smooth(method="lm", fill=NA, fullrange=TRUE) +
#' facet_wrap(~set, ncol=2) +
#' theme_bw() +
#' theme(plot.background=element_rect(fill="#f7f7f7")) +
#' theme(panel.background=element_rect(fill="#f7f7f7")) +
#' theme(panel.grid.minor=element_blank()) +
#' # theme(panel.grid.major.y=element_blank()) +
#' theme(panel.grid.major.x=element_line()) +
#' theme(axis.ticks=element_blank()) +
#' theme(panel.border=element_blank()) +
#' theme(legend.position="top")

#'
#' @export
theme_pub <- function(legend = 'bottom',
                      base_size = 13,
                      base_family = "",
                      horizontal = FALSE,
                      line_width = .5,
                      axis_line = FALSE) {
  half_line <- base_size / 2
  theme(
    # Elements in this first block aren't used directly, but are inherited
    # by others
    line = element_line(
      colour = "#525252",
      size = 0.5, linetype = 1,
      lineend = "butt"
    ),
    rect = element_rect(
      fill = "transparent",
      colour = NA,
      size = 0.5, linetype = 1
    ),
    text =  element_text(
      family = base_family, face = "plain",
      colour = "black", size = base_size,
      lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0,
      margin = ggplot2::margin(), debug = FALSE
    ),

    axis.line =          if(axis_line){element_line()} else{element_blank()},
    axis.text =          element_text(size = rel(0.8), face = "bold", colour = "grey15"),
    axis.text.x =        element_text(margin = ggplot2::margin(t = 0.8 * half_line / 2), vjust = 1),
    axis.text.y =        element_text(margin = ggplot2::margin(r = 0.8 * half_line / 2), hjust = 1),
    axis.ticks =         element_line(),
    axis.ticks.length =  grid::unit(half_line / 2, "pt"),
    axis.title =          element_text(size = rel(0.8), face = "bold"),
    axis.title.x =       element_text(margin = ggplot2::margin(
      t = 0.8 * half_line, b = 0.8 * half_line / 2
    )),
    axis.title.y =       element_text(angle = 90, margin = ggplot2::margin(r = 0.8 * half_line, l = 0.8 * half_line / 2)
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
    panel.grid.major =   element_blank(),
    panel.grid.minor =   element_blank(),
    panel.margin =       grid::unit(half_line, "pt"),
    panel.margin.x =     NULL,
    panel.margin.y =     NULL,
    panel.ontop    =     FALSE,

    strip.background =   element_rect(fill = "grey85", colour = NA),
    strip.text =         element_text(size = rel(0.8), face = "bold", colour = "grey15"),
    strip.text.x =       element_text(margin = ggplot2::margin(t = half_line, b = half_line)),
    strip.text.y =       element_text(angle = -90, margin = ggplot2::margin(l = half_line, r = half_line)
    ),
    strip.switch.pad.grid = grid::unit(0.1, "cm"),
    strip.switch.pad.wrap = grid::unit(0.1, "cm"),

    plot.background =    element_rect(colour = "transparent"),
    plot.title =         element_text(
                                      size = rel(1.1), hjust = 0,
                                      margin = ggplot2::margin(b = half_line * 1.2)),
    plot.subtitle = element_text(size = rel(0.85),
                                 hjust = 0, margin = margin(b = half_line * 0.9)),
    plot.caption = element_text(size = rel(0.9), hjust = 1,
                                margin = margin(b = half_line * 0.9)),
    plot.margin =        margin(half_line, half_line, half_line, half_line),

    complete = TRUE
  )
}
NULL




#' @title Themes for ggplot2 Graphs
#'
#' @description  Theme for plotting  with ggplot2.
#'
#' @param legend Enables to set legend position, default is "none".
#' @param legend_title Will the legend have a title?, Default is \code{FALSE}.
#' @param base_family Default font family.
#' @param base_size Overall font size. Default is 13.
#' @param horizontal Logical. Horizontal axis lines?
#' @param colors Default colors used in the plot in the following order: background, lines, text, and title.
#' @family themes 538
#' @return The theme.
#'
#' @examples
#' qplot(1:10, (1:10)^3) + theme_fte()
#'
#' # Easy to set different theme colors:
#' mycolors = c("#F2F1E8",  "#D2D2D2",  "#6E6E6E", "#6E6E6E")
#' qplot(1:10, (1:10)^3) +
#'  theme_fte(colors=mycolors) #ae8b38
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
                       colors = c('#F0F0F0', '#D9D9D9', '#60636A', '#525252')
                      ) {
  half_line <- base_size / 2
  theme(
    # Elements in this first block aren't used directly, but are inherited
    # by others
    line = element_line(
      colour = colors[2],
      size = 0.5, linetype = 1,
      lineend = "butt"
    ),
    rect = element_rect(
      fill = colors[1],
      colour = colors[1],
      size = 0.5, linetype = 1
    ),
    text = element_text(
      family = base_family, face = "bold",
      colour = colors[3], size = base_size,
      lineheight = 1, hjust = 0.5, vjust = 0.5, angle = 0,
      margin = ggplot2::margin(), debug = FALSE
    ),

    axis.line =          element_blank(),
    axis.text =          element_text(size = rel(1)),
    axis.text.x =        element_text(margin = ggplot2::margin(t = 0.8 * half_line / 2), vjust = 1, size = rel(0.9)),
    axis.text.y =        element_text(margin = ggplot2::margin(r = 0.8 * half_line / 2), hjust = 1, size = rel(0.9)),
    axis.ticks.y =        element_line(color = colors[2]),
    axis.ticks.x =        element_line(color = colors[2]),
    axis.ticks.length =  grid::unit(half_line / 2, "pt"),
    axis.title =          element_text(size = rel(0.8), color = colors[3]),
    axis.title.x =       element_text(vjust = 0, margin = ggplot2::margin(
      t = 0.8 * half_line, b = 0.8 * half_line / 2)),
    axis.title.y =       element_text(angle = 90, vjust = 1.25, margin = ggplot2::margin(r = 0.8 * half_line, l = 0.8 * half_line / 2)
    ),

    legend.background =  element_rect(linetype = 0),
    legend.margin =      grid::unit(0.2, "cm"),
    legend.key =         element_rect(linetype = 0),
    legend.key.size =    grid::unit(1.2, "lines"),
    legend.key.height =  NULL,
    legend.key.width =   NULL,
    legend.text =        element_text(size = rel(0.85)),
    legend.text.align =  NULL,
    legend.title =       if(legend_title){element_text(size = rel(0.8), hjust = 0)} else {element_blank()},
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
    strip.text =         element_text(colour = "grey15",face="plain", size = rel(0.8)),
    strip.text.x =       element_text(margin = ggplot2::margin(t = half_line, b = half_line)),
    strip.text.y =       element_text(
  angle = -90, margin = ggplot2::margin(l = half_line, r = half_line)
    ),
    strip.switch.pad.grid = grid::unit(0.1, "cm"),
    strip.switch.pad.wrap = grid::unit(0.1, "cm"),

    plot.background =    element_rect(),
    plot.title =         element_text(family = "sans",
                                      size = rel(1.1), hjust = 0,
                                      margin = ggplot2::margin(b = half_line * 1.2)),
  plot.subtitle = element_text(size = rel(0.85),
                               hjust = 0, margin = margin(b = half_line * 0.9)),
  plot.caption = element_text(size = rel(0.9), hjust = 1,
                              margin = margin(b = half_line * 0.9)),
    plot.margin =        margin(half_line, half_line, half_line, half_line),

    complete = TRUE
  )
}

#' @export
`theme_538` <- theme_fte



#' @title Create a Completely Empty Theme
#'
#' @description The theme created by this function shows nothing but the plot panel.
#' @inheritParams ggplot2::theme_grey
#' @param font_family Default font family.
#' @param font_size Overall font size. Default is 12.
#' @param legend the legend position.
#' @family themes blank
#' @return The theme.
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
`theme_blank` <- function(font_size = 12, font_family = "serif", legend="none") {
ret <- (theme_grey(base_size = font_size, base_family = font_family) +
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
#' @param font_family Default font family.
#' @param font_size Overall font size. Default is 12.
#' @param legend the position of the legend if any.
#' @family themes darkside
#' @return The theme.
#' # plot with small amount of remaining padding
#' qplot(1:10, (1:10)^2, color="green") + theme_darkside()
#' # Check that it is a complete theme
#' attr(theme_darkside(), "complete")
#'
#' @export
theme_darkside = function(font_size = 12, font_family = "serif", legend="none") {
ret  <- (theme_bw(base_size = font_size) +
    theme(
      text = element_text(family = font_family, color = "grey80"),
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
