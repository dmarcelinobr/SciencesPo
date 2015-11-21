#' @encoding UTF-8
#' @title Add Footnote to a ggplot Object
#'
#' @description Add footnotes to \pkg{ggplot2} objects.
#'
#' @param study any text or empty to use default.
#' @param size the font size \code{study}.
#' @param rotation the rotation for the footnote, default is \code{rotation=90}.
#' @param color the color for \code{study}.
#' @param just the justification method.
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
#' footnote(size = .9, color = "black")
#'
#' @keywords Graphs
#'
#' @import ggplot2
#'
#' @export
#'
`footnote` <-
  function(study=NULL, size=NULL, color=NULL, rotation = 0, just = c("right", "bottom")) {
    if(!is.null(study)){
      text = paste(study)
    } else{
      text = paste(Sys.info()["user"],
                   format(Sys.time(), "%d %b %Y"), sep = " " )
    }
    if(is.null(size)){
      size = .75
    }
    if(is.null(color)){
      color = grDevices::grey(.65)
    }
    grid::pushViewport(viewport())
    grid::grid.text(label = text ,
              x = grid::unit(1,"npc") - unit(1.5, "mm"),
              y = grid::unit(1.5, "mm"),
              just = just,
              rot = rotation,
              gp = grid::gpar(cex = size, col = color))
    grid::popViewport()
  }
NULL
