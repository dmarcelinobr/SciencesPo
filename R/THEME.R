#' Theme for mapping  with ggplot
#' @param base_size Enables to set the font size of all text elements.
#' @param base_family Enables to set the font family of all text elements.
#'
#' @importFrom ggplot2 element_rect element_line element_text element_blank theme theme_bw %+replace%
#' @importFrom grid unit
#' @export
theme_map = function(base_size=9, base_family="")
{
  theme_osa(base_size=base_size, base_family=base_family) %+replace%
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.justification = c(0,0), # bottom of box
          legend.position      = c(0,0), # bottom of picture
          panel.background=element_blank(),
          panel.border=element_rect(colour = "grey90", size = 1, fill = NA),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.margin=unit(0, "lines"),
          plot.background=element_blank()
    )
}

#' Theme for mapping  with ggplot
#' @param base_size Enables to set the font size of all text elements.
#' @param base_family Enables to set the font family of all text elements.
#'
#' @importFrom ggplot2 element_rect element_line element_text element_blank theme theme_bw %+replace%
#' @importFrom grid unit
#' @export
theme_osa = function(base_size=9, base_family="")
{
  theme_bw(base_size=base_size, base_family=base_family) %+replace%
    theme(axis.title.x=element_text(vjust=0),
          axis.title.y=element_text(angle=90, vjust=0.3),
          axis.text=element_text(),
          axis.ticks=element_line(colour="black", size=0.25),
          legend.background=element_rect(fill=NA, colour=NA),
          legend.direction="vertical",
          legend.key=element_rect(fill=NA, colour="white"),
          legend.text=element_text(),
          legend.title=element_text(face="bold", hjust=0),
          panel.border=element_rect(fill=NA, colour="black"),
          panel.grid.major=element_line(colour="grey92", size=0.3, linetype=1),
          panel.grid.minor=element_blank(),
          plot.title=element_text(vjust=1),
          strip.background=element_rect(fill="grey90", colour="black", size=0.3),
          strip.text=element_text()
    )
}
