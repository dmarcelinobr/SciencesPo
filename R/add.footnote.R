#' @title Footnote to ggplot Graphs
#' 
#' @description Add footnotes to \pkg{ggplot2} objects. 
#' 
#' @param text Any text or empty to use default 
#' 
#' @param size The size for text.
#' 
#' @param color Color for the text
#' 
#' @details Only works with a ggplot object.
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' 
#' @examples
#' # setup data
#' x <- seq(0, 50, 1)
#' supply <- x * -2 + 100
#' demand <- x * 2
#' df <- data.frame( x = x, supply=supply, demand=demand)
#'
#' library(ggplot2)
#' ggplot(df, aes(x)) + 
#' geom_line(aes(y = supply, colour = supply))+ 
#' geom_line(aes(y = demand, colour = demand))
#' add.footnote()
#'
#' @keywords Graphs
#' 
#' @importFrom ggplot2 ggplot
#' @importFrom grid unit
#' @importFrom grid pushViewport
#' @importFrom grid popViewport
#' @importFrom grid grid.text
#' @importFrom grid gpar
#' @importFrom grid viewport
#'
#' @export
#'
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
