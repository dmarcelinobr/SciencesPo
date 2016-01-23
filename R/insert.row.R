#' @title Add New Row to a data.frame
#' @description Facilitates inserting new rows in a data.frame.
#' @param .data The existing data.frame
#' @param row The new row to be appended.
#' @param where An integer for the position to add the row, default is at the top of the data.frame.
#'
#' @examples
#' existingDF <- as.data.frame(matrix(seq(20),nrow=5,ncol=4))
#' existingDF
#' r <- 3
#' newrow <- seq(4)
#'
#' insert.row(existingDF, newrow, r)
#'
#' @export
insert.row <- function(.data, row, where=1){
  .data[seq(where+1,nrow(.data)+1),] <- .data[seq(where,nrow(.data)),]
  .data[where,] <- row
  .data
}
NULL

