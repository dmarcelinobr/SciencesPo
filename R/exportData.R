#' @encoding UTF-8
#' @title  Write a tab separated file tsv
#'
#' @description Write a tab separated tsv, use tab as seperator
#'
#' @param dataset the filename where to write the csv
#'
#'
#'
#'@examples
#'df = data.frame(id=1:20, x=rnorm(20, mean=2, sd=.5), y=rnorm(20, mean=5, sd=2))
#' exportData(df)
#'
#' @export
exportData <- function(dataset) {
  filename = paste(Sys.Date(),".txt", sep="")
  source <- dataset
  # write a tab separated tsv, use tab as seperator
  write.table(source, filename, sep="\t", row.names=FALSE, col.names=TRUE)
}