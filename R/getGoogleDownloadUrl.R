#' Construct a google sheet download url from the sheet's viewing url
#'
#' Converts the viewing url for a google sheet to a download url.
#'
#' @param url the google sheet url
#' @param format controls the column separator used. \code{csv} or \code{tsv}
#' @param sheetid the id of the sheet to download from.  (Default \code{NULL}, downloads the first sheet)
#'
#' @export
#' @importFrom readr read_csv
#'
#' @examples
#'
#' # Download a sheet manually using readr
#' url <- 'docs.google.com/spreadsheets/d/1I9mJsS5QnXF2TNNntTy-HrcdHmIF9wJ8ONYvEJTXSNo'
#'
#' if(requireNamespace('readr', quietly=TRUE)){
#'   library(readr)
#'   read_csv(getGoogleSpreadsheetUrl(url), col_types = cols(
#'     mpg = col_double(),
#'     cyl = col_integer(),
#'     disp = col_double(),
#'     hp = col_integer(),
#'     drat = col_double(),
#'     wt = col_double(),
#'     qsec = col_double(),
#'     vs = col_integer(),
#'     am = col_integer(),
#'     gear = col_integer(),
#'     carb = col_integer()
#'   ))
#' }
#'
`getGoogleSpreadsheetUrl` <- function(url, format='csv', sheetid = NULL){
  key <- stringr::str_extract(url, '[[:alnum:]_-]{30,}')
  if(is.null(sheetid) & stringr::str_detect(url, 'gid=[[:digit:]]+')){
    sheetid <- as.numeric(stringr::str_extract(stringr::str_extract(url,'gid=[[:digit:]]+'),'[[:digit:]]+'))
  }
  address <- paste0('https://docs.google.com/spreadsheets/export?id=',key,'&format=',format)
  if(!is.null(sheetid)){
    address <- paste0(address, '&gid=', sheetid)
  }
  return(address)
}





#' Download Google sheet as a table
#'
#' This is a convenience function, designed to download a table quickly and conveniently.
#'
#' @details
#' The Google sheet must have 'share by link' turned on.
#'
#' If the package \code{readr} is available, then it will be used.
#' This can produce slightly different, but normally better, parsings.
#'
#'
#' @param url the google sheet url
#' @param sheetid the index of the sheet to be downloaded. If you use the direct sheet URL, rather than the share by link, this will automatically be extracted.
#' Otherwise, the first sheet will be downloaded by default.
#'
#' @export
#' @importFrom readr read_csv
#'
#' @examples
#'
#' # Download a sheet
#' url <- 'docs.google.com/spreadsheets/d/1I9mJsS5QnXF2TNNntTy-HrcdHmIF9wJ8ONYvEJTXSNo'
#' a <- getGoogleSpreadsheet(url)
#'
#' # Download the second sheet, using the direct url
#' url <- 'docs.google.com/spreadsheets/d/1I9mJsS5QnXF2TNNntTy-HrcdHmIF9wJ8ONYvEJTXSNo#gid=850032961'
#' b <- getGoogleSpreadsheet(url)
#'
#'
#' # Or, with readr:
#' if(requireNamespace('readr', quietly=TRUE)){
#'   library(readr)
#'   read_csv(getGoogleSpreadsheetUrl(url), col_types = cols(
#'     mpg = col_double(),
#'     cyl = col_integer(),
#'     disp = col_double(),
#'     hp = col_integer(),
#'     drat = col_double(),
#'     wt = col_double(),
#'     qsec = col_double(),
#'     vs = col_integer(),
#'     am = col_integer(),
#'     gear = col_integer(),
#'     carb = col_integer()
#'   ))
#' }
#'
`getGoogleSpreadsheet` <- function(url, sheetid = NULL){
  if(requireNamespace('readr', quietly=TRUE)){
    suppressMessages(table <- readr::read_csv(file = getGoogleSpreadsheetUrl(url, format='csv', sheetid = NULL)))
  }else{
    stop("Unable to load package `readr`; please install it and try again.")
    class(table) <- c("tbl_df", "tbl", "data.frame")
  }
  return(table)
}
