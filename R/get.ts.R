get.ts <-
function (timevar, format, data = NULL, tz = "GMT") 
{
  dimensions <- dim(data)
  clss <- lapply(data, class)
  if (!is.null(data)) {
    if (!is.null(dimensions[1])) {
      if (length(timevar) != length(data[, 1])) 
        stop("Different lengths in data and date variables")
    }
    else {
      if (length(timevar) != length(data)) 
        stop("Different lengths in data and date variables")
    }
  }
  y <- (data)
  parms <- (strptime(paste(timevar), format, tz = tz))
  second <- second(parms)
  minute <- minute(parms)
  hour <- hour(parms)
  day <- day(parms)
  week <- week(parms)
  month <- month(parms)
  year <- year(parms)
  date <- ymd(parms)
  if (is.null(parms)) {
    answer <- data.frame(date,year,month,week,day,hour,minute,second)
  }
  else {
    answer <- data.frame(date, year,month,week,day,hour,minute,second, y)
  }
  return(answer)
}
