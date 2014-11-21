theme_black <- function (base_size = 12, base_family = "") {
  theme_gray(base_size = base_size, base_family = base_family) %+replace% 
    theme(
      axis.text = element_text(colour = "white"),
      axis.title.x = element_text(colour = "white"),
      axis.title.y = element_text(colour = "white", angle=90),
      panel.background = element_rect(fill="grey5"),
      panel.grid.minor.y = element_line(),
      panel.grid.major = element_line(colour = "grey50"),
      plot.background = element_rect(fill="grey5")
    )   
}