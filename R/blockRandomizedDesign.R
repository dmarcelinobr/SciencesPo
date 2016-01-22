#' @encoding UTF-8
#' @title Create a Block-randomized design
#'
#' @description Generate block-randomized designs based on the number of units \code{n} and block-size, where the block-size is the number of experimental conditions. The number of Independent Variables and the number of levels in each IV are specified as input. The output is a the block randomized design. This function is intended for planning randomized trails.
#'
#' @param blocksize is the number of control blocks or n per block/group.
#' @param n is the total number of subjects or units.
#' @param seed the random number generation seed.
#'
#' @references
#' Alan S Gerber, Donald P Green (2012). \emph{Field experiments: Design, analysis, and interpretation}. WW Norton.
#'
#' RB Morton, KC Williams (2010). \emph{Experimental political science and the study of causality: From nature to the lab}. Cambridge University Press.
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#'
#' blk <- rbd(blocksize = 10, n = 40, seed = 51)
#'
#' blk;
#'
#' crosstable(blk, block, condition)
#'
#'@export
#'
#' @importFrom stats runif
#'
`rbd` = function(blocksize, n, seed=NULL){
  if(!is.null(seed)){
    set.seed(seed)
  }
  # blocking factor
  block = rep(1:ceiling(n/blocksize), each = blocksize)
  a1 = data.frame(id=1:length(block), block, rand=runif(length(block)))
  a2 = a1[order(a1$block,a1$rand),]
  # matching treatment
  a2$condition = rep(c("Treat", "Control"),times = length(block)/2)
  assign = a2[order(a2$id),]
  class(assign) <- c("SciencesPo", "random", "data.frame")
  return(assign)
}
NULL
