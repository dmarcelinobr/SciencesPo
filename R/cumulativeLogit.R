#' @encoding UTF-8
#' @title Cumulative Logit
#'
#' @param y The dependent variable.
#' @param adj The adjustment constant.
#' @author Daniel Marcelino \email{dmarcelino@@live.com}.
#'@export
`cumulativeLogit` <- function(y, adj = 0.5) {
  ncol <- dim(y)[2]
  y <- t(apply(y, 1, cumsum))
  log((y[,-ncol] + adj)/(y[,ncol] - y[,-ncol] + adj))
}
NULL
