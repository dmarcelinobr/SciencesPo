#' @encoding UTF-8
#' @title Yearly aggregation
#' 
#' @description Yield for yearly aggregation. It does handle discontinuous time series by years. The user can also set its own method by passing an ad-hoc function for the aggregation method.
#'
#' @param data A time-series object
#' @param FUN A function to aggregate data
#' @param years A numeric value for the number of years to perform aggregation
#' @param na.rm A logical value for na.rm, default is \code{FALSE}
#' @param civil Wether the years start from the civil date or from the first observation in data. The default is \code{civil = FALSE}
#'
#' @details A data.frame of type long with dates formatted as as.Date and aggregated data 
#' 
#' @references  Ripley, B. D. and Hornik, K. (2001) Date-time classes. \emph{R News}, 1/2, 8–11 \url{http://www.r-project.org/doc/Rnews/Rnews_2001-2.pdf}
#' 
#' @seealso \link{by_day}, \link{by_month}
#' 
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' 
#' @keywords Data-Manipulation
#' 
#' @examples  
#' data(us2012)
#' \dontrun{Obama.ts <- as.tsdf(us2012[,4], '%Y-%m-%d  %H:%M:%S', us2012[,8])
#' # yearly aggregated means for data:
#'  yearly <-by_year(Obama.ts, mean)
#' # civil year arrangement: 
#' yearly <-by_year(Obama.ts, mean, civil=TRUE)
#' # Means aggregated for data:
#' yearly <-by_year(Obama.ts, mean, 2) }
#' 
#' @export
#' 
by_year <- function (data, FUN, years = NULL, na.rm = FALSE, civil = FALSE) 
{
    if (is.null(years)) {
        years = 1
    }
    start <- min(data$year)
    end <- max(data$year)
    if ((end - start)%%years != 0) 
        warning("Last gap does not contain ", years, " years. There is only ", 
            ((end - start)%%years), " year(s) in this gap.", 
            call. = FALSE)
    if (civil == FALSE) {
        yrs <- (as.numeric(difftime(data$date, data$date[1], 
            units = c("hours"))) - as.numeric(difftime(data$date, 
            data$date[1], units = c("hours")))%%8765.81277)/8765.81277
        years.again <- yrs - yrs%%years
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
        years.again <- years - years%%years
        data$year <- data$year[1] + years.again
        date.1 <- as.Date(strptime(paste(1, 1, data$year), "%d %m %Y"))
        new.1 <- data.frame(date = date.1, data[, 8:length(data)])
        answer <- aggregate(new.1[, 2:length(new.1)], list(date = new.1$date), 
            FUN, na.rm = na.rm)
        names(answer) <- c("Date", names(data[8:length(data)]))
    }
    return(answer)
}
