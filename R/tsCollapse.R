#' @encoding UTF-8
#'   @title Aggregated Data by Time Parameters
#' 
#' @description Return a \code{data.frame} with aggregated data by time parameters. In order to use this function, you have to have a \link{as.timedf} data.frame.
#' 
#' @param data a \bold{timedf} object
#' @param by the method used to break the dependent variable \code{y} in \code{data}; three options \code{"day","month",or "year"}.
#' @param FUN a function to aggregate data \code{sum, mean, min, max, etc}.
#' @param factor is the aggregator factor; a numeric value representing days, months or years to perform aggregation by.
#' @param na.rm A logical value for \code{na.rm}, default is \code{FALSE}
#' @param civil Wether the years start from the civil date or from the first observation in data. The default is \code{civil = FALSE}.
#' @param plot whether the data.frame to be plotted, default is \code{plot = TRUE}.
#' 
#' @details A data.frame of type long with summarized time series data (y) and time parameters formatted as POSIXct. 
#' @note To weekly aggregation, set \code{by="day"} and \code{factor = 7}
#' 
#' @references  Ripley, B. D. and Hornik, K. (2001) Date-time classes. \emph{R News}, 1/2, 8–11 \url{http://www.r-project.org/doc/Rnews/Rnews_2001-2.pdf}
#' 
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' 
#' @keywords Data-Manipulation
#' 
#' @examples  
#' data(us2012)
#' Obama.ts <- as.timedf(us2012[,3], '%Y-%m-%d', us2012[,8])
#'
#' # Daily aggregated means for data:
#' daily <- tsCollapse(Obama.ts, by="day", mean)
#' 
#' # Weekly aggregated means for data:
#' weekly <-tsCollapse(Obama.ts, by = "day", factor =7, mean)
#' 
#' # monthly aggregated means for data:
#' monthly <-tsCollapse(Obama.ts, by = "month", mean)
#'
#' # bimonthly or semimonthly aggregated means for data:
#' bimonthly <-tsCollapse(Obama.ts,by = "month", mean, factor = 2)
#' 
#' @export
#' 
tsCollapse <-
function (data,  by = c("day","month","year"), FUN, na.rm = FALSE, factor = NULL, civil = FALSE, plot = TRUE) 
{
  if (!inherits(data, c("timedf") )) stop("object not of class \"timedf\". Please, use as.timedf() before proceed.")
	  #if (by != c("day","month","year")) stop("a method for `by` should be supplied.")
		  
  ## by Day
  if (by =="day"){
  if (is.null(factor)) {
    factor = 1
  }
  ymd <- function(x) {
      as.Date(x, "%Y %m %d")  
  }
  dmy <- function(x) {
      as.Date(x, "%d %m %Y")  
  }
  if (factor == 1) {
    day <- aggregate(data[, 8:length(data)], list(day = data$days, 
  month = data$months, year = data$years), FUN=FUN, na.rm = na.rm)
    days <- ymd(paste(day$year, day$month, day$day))
    answer <- data.frame(date = days, data = day[, 5:length(day)])
    names(answer) <- c("Date", names(data[9:length(data)]))
    return(answer)
  }
  temp <- data
  day <- aggregate(list(data[, 8:length(data)], count = 1), 
                   list(day = data$days, month = data$months, year = data$years), 
                   FUN=FUN, na.rm = na.rm)
  days <- ymd(paste(day$year, day$month, day$day))
  data <- data.frame(date = days, day[, 5:length(day) - 1], 
                     count = day[length(day)])
  days = paste(factor, "days")
  all.dates <- seq.Date(as.Date(data$date[1]), as.Date(data$date[length(data[, 
   1])]), by = "days")
  dates <- data.frame(date = all.dates)
  aggreg <- merge(dates, data, by = "date", all.x = TRUE)
  aggreg$date <- rep(seq.Date(as.Date(data$date[1]), as.Date(data$date[length(data[, 1])]), by = days), each = factor, length = length(all.dates))
  answer <- aggregate(list(aggreg[3:length(aggreg)]), 
                       list(date = aggreg$date), FUN = FUN, na.rm = TRUE)
  answer <- subset(answer, answer$count != 0)
  answer <- answer[, -length(answer)]
  names(answer) <- c("Date", names(temp[9:length(temp)]))
  class(answer) <- c("SciencesPo", "data.frame")
  
}
## by month
 if (by =="month"){
	 if (is.null(factor)) {
	     factor = 1
	   }
	   month <- aggregate(data[, 8:length(data)], list(month = data$months, 
	   year = data$years), FUN=FUN, na.rm = na.rm)
	  # data.cols <- length(month)
	   if (factor > 1) {
	     month.gap <- month[, 1]
	     for (i in 1:length(month[, 1])) {
	       month.gap[i] = (month[i, 2]%%month[1, 2]) * 12 + 
	         month[i, 1]
	     }
	     month.gap <- month.gap - month.gap%%factor
	     month.gap <- month.gap%%12 + 1
	     year <- month[, 2]
	     if (sum(month.gap) == length(month.gap)) {
	       year <- year[1] + (year - year[1]) - (year - year[1])%%( factor /12)
	     }
	     else {
	       for (i in 2:length(month.gap)) {
	         if (month.gap[i] == month.gap[i - 1]) 
	           year[i] = year[i - 1]
	         else year[i] = month[i, 2]
	       }
	     }
	     date = strptime(paste(1, month.gap, year), "%d %m %Y")
	     answer0 <- data.frame(date, data = month[, 3:length(month)])
	     answer <- aggregate(answer0[, 2:length(answer0)], list(date = answer0$date), 
	                        FUN=FUN, na.rm = na.rm)
	     names(answer) <- c("Date", names(data[9:length(data)]))
	     return(answer)
	   }
	   else {
	     date = strptime(paste(1, month$month, month$year), "%d %m %Y")
	     answer <- data.frame(date, data = month[, 3:length(month)])
	     names(answer) <- c("Date", names(data[9:length(data)]))
	     class(answer) <- c("SciencesPo", "data.frame")
	     return(answer)
	   }	 
 }
 
 # By year
 
 if (by =="year"){
	 if (is.null(factor)) {
	        factor = 1
	    }
	    start <- min(data$year)
	    end <- max(data$year)
	    if ((end - start)%%factor != 0) 
	        warning("Last gap does not contain ", years, " years. There is only ", 
	            ((end - start)%%factor), " year(s) in this gap.", 
	            call. = FALSE)
	    if (civil == FALSE) {
	        yrs <- (as.numeric(difftime(data$date, data$date[1], 
	            units = c("hours"))) - as.numeric(difftime(data$date, 
	            data$date[1], units = c("hours")))%%8765.81277)/8765.81277
	        years.again <- yrs - yrs%%factor
	        data$year <- data$year[1] + years.again
	        date.1 <- as.Date(strptime(paste(data$day[1], data$month[1], 
	            data$year), "%d %m %Y"))
	        new.1 <- data.frame(date = date.1, data[, 8:length(data)])
	        answer <- aggregate(new.1[, 2:length(new.1)], list(date = new.1$date), 
	            FUN, na.rm = na.rm)
	        names(answer) <- c("Date", names(data[8:length(data)]))
	    }
	    else {
	        years <- data$year - data$year[1]
	        years.again <- years - years%%factor
	        data$year <- data$year[1] + years.again
	        date.1 <- as.Date(strptime(paste(1, 1, data$year), "%d %m %Y"))
	        new.1 <- data.frame(date = date.1, data[, 8:length(data)])
	        answer <- aggregate(new.1[, 2:length(new.1)], list(date = new.1$date), 
	            FUN, na.rm = na.rm)
	        names(answer) <- c("Date", names(data[8:length(data)]))
	    }
	    class(answer) <- c("SciencesPo", "data.frame")
	    return(answer)	 
 }
 
}
