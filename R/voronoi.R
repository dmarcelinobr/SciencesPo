#' @title Voronoi diagram
#'
#' @description Computes the voronoi diagram.
#'
#' @param p An integer for the size of the
#' @param n An integer for the size of
#' @param dim The dimension of the image.
#' @details
#' \url{https://en.wikipedia.org/wiki/Voronoi_diagram}
#' @importFrom data.table :=
#' @export
#' @examples
#' voronoi(p=2, n=20, dim=1000)
#'
voronoi <- function(p, n=100, dim=1000){
  dim.image <- dim
  colors <- grDevices::rainbow(n)
  points <- data.frame(id = 1:n, x0 = runif(n) * dim, y0 = runif(n) * dim)
  tmp <- data.table::data.table(expand.grid(x = 1:dim.image,
                                y = 1:dim.image, id = 1:n), key = "id")
  tmp <- merge(tmp, points, by = "id")

  .distancia <- function(a, b, c, d, p)
    (abs(a-c)^p + abs(b-d)^p)^(1/p)

  tmp$distancia <- .distancia(tmp$x, tmp$y, tmp$x0, tmp$y0, p)
  tmp[, rank := rank(distancia, ties.method = "random"), by = c("x", "y")]

  rejilla <- tmp[tmp$rank == 1,]
  rejilla$x0 <- rejilla$y0 <- rejilla$distancia <- rejilla$rank <- NULL

  rejilla$color <- colors[rejilla$id]

  imagen <- as.matrix(reshape2::dcast(rejilla, x ~ y, value.var = "color")[,-1])

  grid::grid.raster(imagen)
}
NULL
