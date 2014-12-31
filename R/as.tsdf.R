#' @encoding UTF-8
#' @title A data.frame with time series parameters
#' 
#' @description Return a \code{data.frame} with time parameters and time variable
#' 
#' @param timevar A time variable
#' @param format A time format
#' @param x Data variable to convert to a time series
#' @param tz Time zone code, default is \dQuote{GMT}
#' 
#' @details A data frame containing time variable parameters and measuring variable
#' 
#' @references  Ripley, B. D. and Hornik, K. (2001) Date-time classes. \emph{R News}, 1/2, 8–11 \url{http://www.r-project.org/doc/Rnews/Rnews_2001-2.pdf}
#' 
#' @seealso \link{by_day}, \link{by_month}, \link{by_year}
#' 
#' @keywords Data-Manipulation
#' 
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' 
#' @examples  
#' data(ssex)
#' myts <- as.tsdf(timevar = ssex[,1], format = "%Y", x= ssex[,3])
#' peek(myts)
#' 
#' @importFrom lubridate ymd
#' @importFrom lubridate dmy
#' @importFrom lubridate second
#' @importFrom lubridate minute
#' @importFrom lubridate day
#' @importFrom lubridate hour
#' @importFrom lubridate week
#' @importFrom lubridate month
#' @importFrom lubridate year
#' 
#' @export
#' 
as.tsdf <-
function (timevar, format, x = NULL, tz = "GMT") 
{
  dimensions <- dim(x)
  clss <- lapply(x, class)
  if (!is.null(x)) {
    if (!is.null(dimensions[1])) {
      if (length(timevar) != length(x[, 1])) 
        stop("Different lengths in `x` and `time` variables")
    }
    else {
      if (length(timevar) != length(x)) 
        stop("Different lengths in `x` and `time` variables")
    }
  }
  y <- (x)
  parms <- (strptime(paste(timevar), format, tz = tz))
  seconds <- second(parms)
  minutes <- minute(parms)
  hours <- hour(parms)
  days <- day(parms)
  weeks <- week(parms)
  months <- month(parms)
  years <- year(parms)
  date <- ymd(parms)
  if (is.null(parms)) {
    answer <- data.frame(date,years,months,weeks,days,hours,minutes,seconds)
  }
  else {
    answer <- data.frame(date, years,months,weeks,days,hours,minutes,seconds, y)
  }
  return(answer)
}
