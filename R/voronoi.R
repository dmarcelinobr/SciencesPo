#' @title Voronoi tessellation diagram
#'
#' @description Computes voronoi tessellation diagrams, and Dirichlet tessellation (after Peter Gustav Lejeune Dirichlet).
#'
#' @param n an integer for a finite set of points.
#' @param d an integer for a finite set of dimensions.
#' @param dim the image dimension.
#' @param method the distance computation method. One of \code{euclidean, manhattan, maximum, canberra} (currently not implemented).
#' @param seed an integer for random seed.
#' @param plot logical. If \code{TRUE}, a plot is returned, else, a \code{data.frame} is returned.
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @details
#' \url{https://en.wikipedia.org/wiki/Voronoi_diagram}
#' @importFrom data.table := setDT dcast.data.table
#' @export
#' @examples
#' \dontrun{ Voronoi(n=20, d=5, dim=1000) }
#'
`Voronoi` <- function(n=100, d, dim=1000, method=NULL, seed=51, plot=TRUE){
  s1 <- Sys.time()
  dim.image <- dim
  colors <- grDevices::rainbow(n)
  set.seed(seed)
  points <- data.frame(id = 1:n, x0 = runif(n) * dim, y0 = runif(n) * dim)
  tmp <- data.table::data.table(expand.grid(x = 1:dim.image,
                                            y = 1:dim.image, id = 1:n), key = "id")
  tmp <- merge(tmp, points, by = "id")

`.distance` <- function(a1, b1, a2, b2, d){
    (abs(a1-a2)^d + abs(b1-b2)^d)^(1/d)}
  # R check barked on global variable on distance.
  distance <-NULL
  tmp$distance <- .distance(tmp$x, tmp$y, tmp$x0, tmp$y0, d)
  tmp[, rank := rank(distance, ties.method = "random"), by = c("x", "y")]
  frame <- tmp[tmp$rank == 1,]
  if(plot==TRUE){
    frame[,4:7] <-NULL
    frame$color <- colors[frame$id]
    imagen <- as.matrix(data.table::dcast.data.table(data.table::setDT(frame), x ~ y, value.var = "color")[,-1, with=FALSE])
    graphics::frame()
    grid::grid.raster(imagen)
    s2 <- Sys.time()
    timediff <- c( s2 - s1 )
    cat("\n")
    cat("Date of Analysis: ",format(Sys.time(), "%a %b %d %Y"), "\n", "Computation time: ",timediff,sep="","\n")
    cat("-----------------------------------\n")
  }else{
    s2 <- Sys.time()
    timediff <- c( s2 - s1 )
    cat("\n")
    cat("Date of Analysis: ",format(Sys.time(), "%a %b %d %Y"), "\n", "Computation time: ",timediff,sep="","\n")
    cat("-----------------------------------\n")
    return(frame)
  }
}### end -- voronoi function
NULL
