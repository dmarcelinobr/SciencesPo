#' @encoding UTF-8
#' @title Identify columns with at least one NA value and fill with the mean value
#'
#' @param x a \code{data.frame}
#'
#' @return A message indicating whether any column in \code{x} has missing data and filling missings with the mean of the valid cases.
#' @author Daniel Marcelino \email{dmarcelino@@live.com}
#' @examples
#'     data(ssex)
#'     ssex_fixed <-fillMissingWithMean(ssex)
#' @export
`fillMissingWithMean` <- function(x){
  for (i in 1:ncol(x))  {
    if (sum(is.na(x[,i])) > 0 ) {
      print(paste("column",i,"has missing data"))
      mean.col <- mean(x[,i], na.rm=T)
      for (j in 1:nrow(x))  {
        if (is.na(x[j,i]) ==T)
          x[j,i] <- mean.col
      }
    }
  }
  invisible(x)
}
NULL
