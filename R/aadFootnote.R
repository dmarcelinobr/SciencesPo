#' @title Add Footnote to a ggplot Object
#'
#' @description Add footnotes to \pkg{ggplot2} objects.
#'
#' @param x any text or empty to use default.
#' @param size the font size \code{x}.
#' @param rotation the rotation for the footnote, default is \code{rotation=90}.
#' @param color the color for \code{x}.
#' @param justification the justification method.
#'
#' @details At this stage, this function only works for a ggplot object.
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
#' aadFootnote(size = .9, color = "red")
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
aadFootnote <-
function(x=NULL, size=NULL, color=NULL, rotation = 90, justification = c("right", "bottom")) {
  if(!is.null(x)){
    text = paste(x)
  } else{ 
  text = paste(Sys.info()["user"],
  format(Sys.time(), "%d %b %Y"),sep = " " ) 
  }
  if(is.null(size)){
    size = .65
  } 
  if(is.null(color)){
    color = grey(.75)
  }
  pushViewport(viewport())
  grid.text(label = text ,
    x = unit(1,"npc") - unit(2, "mm"),
    y = unit(35, "mm"),
    just = justification,
	  rot = rotation,
    gp = gpar(cex = size, col = color))
 popViewport()
}
