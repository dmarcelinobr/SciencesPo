#' Helper function providing startdate and enddate for a given year
#'
#' Standard years
#' @export
#' @param year integer
stdYears <- function(year){
  list(startdate = paste0(year, "-01-01"),
       enddate = paste0(year, "-12-31"),
       startoffset = 0,
       endoffset = 0)
}
