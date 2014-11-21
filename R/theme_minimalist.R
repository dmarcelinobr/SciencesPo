theme_minimalist <-
function (base_size = 12, base_family = "sans", title_family = "mono") 
{

(theme_grey(base_size = base_size, base_family = base_family) + 
 theme(rect = element_rect(fill ='white', linetype = 0, colour = NA),
  axis.text = element_text(size = rel(1), colour ='#3C3C3C'),
  text = element_text(family = base_family, face = "plain", colour ='#3C3C3C', size = base_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = .7),
 #axis.title.x = element_text(), axis.title.y = element_text(angle = 90),
 axis.title = element_blank(),
  axis.ticks.y = element_blank(), axis.ticks.x = element_line(colour = NULL), 
  axis.line = element_line(), axis.line.y = element_blank(), 
             legend.background = element_rect(colour=NA), legend.position = "top", 
             legend.direction = "horizontal", legend.box = "vertical",
  panel.background = element_rect(fill = "white"), 
  panel.grid = element_line(colour = NULL), 
  panel.grid.major = element_line(colour = "grey90"), 
  panel.grid.major.x = element_line(colour='grey90'), panel.grid.minor = element_blank(),
  panel.margin = unit(0.25, "lines"),
  strip.text = element_text(size = rel(1.25)), 
  strip.text.x = element_text(),
  strip.text.y = element_text(),
  plot.background = element_rect(fill ='white'),
  plot.title = element_text(hjust = 0, face = "bold", size = rel(1.25)),
  plot.margin = unit(c(5, 1, 1, 1) * 2, "points"),
  strip.background = element_rect(colour = NULL)))
  }