by.day <-
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
