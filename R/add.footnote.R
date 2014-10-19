add.footnote <-
function(text = paste(Sys.info()["user"],  
                         format(Sys.time(), "%d %b %Y"),sep = " " ),
                         size = .7, color = grey(.75))
{
  pushViewport(viewport())
  grid.text(label = text ,
    x = unit(1,"npc") - unit(2, "mm"),
    y = unit(35, "mm"),
    just = c("right", "bottom"),
	rot = 90,
    gp = gpar(cex = size, col = color))
 popViewport()
}
