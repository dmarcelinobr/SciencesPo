get.ts <-
function (datevar, format, datavar = NULL, tz = "GMT") 
{
  dimensions <- dim(datavar)
  clss <- lapply(datavar, class)
  if (!is.null(datavar)) {
    if (!is.null(dimensions[1])) {
      if (length(datevar) != length(datavar[, 1])) 
        stop("Different lengths in data and date variables")
    }
    else {
      if (length(datevar) != length(datavar)) 
        stop("Different lengths in data and date variables")
    }
  }
  y <- (datavar)
  date <- (strptime(paste(datevar), format, tz = tz))
  minute <- minute(date)
  hour <- hour(date)
  day <- day(date)
  week <- week(date)
  month <- month(date)
  year <- year(date)
  if (is.null(date)) {
    answer <- data.frame(date,year,month,week,day,hour,minute)
  }
  else {
    answer <- data.frame(date, year,month,week,day,hour,minute, y)
  }
  return(answer)
}
