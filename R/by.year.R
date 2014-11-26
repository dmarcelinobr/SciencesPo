by.year <- function (data, FUN, years = NULL, na.rm = FALSE, civil = FALSE) 
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
