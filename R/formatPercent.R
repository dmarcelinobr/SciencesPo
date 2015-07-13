#' @encoding UTF-8
#' @title Converts to percentiles
#' @description Converts a numeric vector to percentiles.
#' @param x a numeric vector.
#' @author Daniel Marcelino \email{dmarcelino@@live.com}
#' @examples
#' vec <- seq(1:5)
#' formatPercent(vec)
#' @export
`formatPercent` <- function(x){
  pt1 <- quantile(x, probs = seq(0, 1, by = 0.01), type = 7)
  pt2 <- unique(as.data.frame(pt1), fromLast = TRUE)
  pt3 <- rownames(pt2)
  pt4 <- as.integer(strsplit(pt3, "%"))
  ans <- pt4[as.integer(cut(x, c(0, pt2$pt1), labels = 1:length(pt3)))]
  return(ans)
}
NULL
