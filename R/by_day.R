#' @encoding UTF-8
#'   @title A data.frame with time series parameters
#' 
#' @description Return a \code{data.frame} with time parameters and time variable
#' 
#' @param data A time-series object
#' @param FUN A function to aggregate data
#' @param days A numeric value for the number of days to perform aggregation
#' @param na.rm A logical value for na.rm, default is \code{FALSE}
#' 
#' @details A data.frame of type long with dates formatted as as.Date and aggregated data
#' @note To weekly aggregation, set \code{days = 7}
#' 
#' @references  Ripley, B. D. and Hornik, K. (2001) Date-time classes. \emph{R News}, 1/2, 8–11 \url{http://www.r-project.org/doc/Rnews/Rnews_2001-2.pdf}
#' 
#' @seealso \link{by_month}, \link{by_year}
#' 
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' 
#' @keywords Data-Manipulation
#' 
#' @examples  
#' data(us2012)
#' \dontrun{Obama.ts <- as.tsdf(us2012[,4], '%Y-%m-%d  %H:%M:%S', us2012[,8])
#' # Daily aggregate means for data:
#' daily <- by_day(Obama.ts, mean)
#' # Weekly aggregated means for data:
#' weekly <-by_day(Obama.ts, mean, 7) }
#' 
#' @importFrom lubridate ymd
#' @importFrom lubridate dmy
#' @export
#' 
by_day <-
function (data, FUN, days = NULL, na.rm = FALSE) 
{
  if (is.null(days)) {
    days = 1
  }
  if (days == 1) {
    day <- aggregate(data[, 9:length(data)], list(day = data$day, 
  month = data$month, year = data$year), FUN, na.rm = na.rm)
    days <- ymd(paste(day$year, day$month, day$day))
    data2 <- data.frame(date = days, data = day[, 4:length(day)])
    names(data2) <- c("Date", names(data[9:length(data)]))
    return(data2)
  }
  ymd <- function(x) {
      as.Date(x, "%Y %m %d")  
  }
  temp <- data
  day <- aggregate(list(data[, 9:length(data)], count = 1), 
                   list(day = data$day, month = data$month, year = data$year), 
                   FUN, na.rm = na.rm)
  days <- ymd(paste(day$year, day$month, day$day))
  data <- data.frame(date = days, day[, 5:length(day) - 1], 
                     count = day[length(day)])
  days = paste(days, "days")
  all.dates <- seq.Date(as.Date(data$date[1]), as.Date(data$date[length(data[, 
   1])]), by = "day")
  dates <- data.frame(date = all.dates)
  aggreGated <- merge(dates, data, by = "date", all.x = TRUE)
  aggreGated$date <- rep(seq.Date(as.Date(data$date[1]), as.Date(data$date[length(data[, 1])]), by = days), each = days, length = length(all.dates))
  answer <- aggregate(list(aggreGated[2:length(aggreGated)]), 
                       list(date = aggreGated$date), FUN, na.rm = TRUE)
  answer <- subset(answer, answer$count != 0)
  answer <- answer[, -length(answer)]
  names(answer) <- c("Date", names(temp[9:length(temp)]))
  return(answer)
}
