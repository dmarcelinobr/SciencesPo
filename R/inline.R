#' Inline data.frame
#'
#' @description Utility function to create of a \code{data.frame} inline.
#'
#' @param str text representation of the data frame
#' @param header see \code{\link{read.table}}
#' @param colClasses see \code{\link{read.table}}
#' @param \dots see \code{\link{read.table}}
#' @examples
#' mydf <- inline(str = "County, Gore, Bush, Buchanan, Nader
#' ALACHUA, 47365, 34124, 263, 3226
#' BAKER,    2392,  5610, 73, 53
#' BAY,     18850, 38637, 248, 828
#' BRADFORD, 3075, 5414, 65, 84
#' BREVARD, 97318, 115185, 570, 4470
#' BROWARD, 386561, 177323, 788, 7101", sep=",")
#'
#' \dontrun{sapply(mydf[, 2 : 5], sum);}
#'
#' \dontrun{cbind(stack(mydf), county = mydf$County);}
#'
#' @export
inline <- function(str,header=TRUE,colClasses=NA, ...){
  utils::read.table( text = str, header=header, colClasses=colClasses, ... )
}
