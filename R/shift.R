#' @title Lag or Lead Observations
#' 
#' @description Shift function allows one to either lag or lead a column variables in a data frame.
#' 
#' @param x the variable to be lagged or leaded
#' @param id the subject or identification variable.
#' @param time the time id variable. 
#' @param delta an integer value (positive or negative) for units to move either backward or forward.
#' 
#' @details The combination of \code{id} and \code{time} must yelds to a unique identification of the observations.
#' 
#' @return An object of the same type as \code{x}
#' 
#' @examples 
#' data(sheston91)
#' attach(sheston91)
#' peek(sheston91)
#' ## lag
#' sheston91$L.pop <- shift(x = pop, id = country, time = year, delta = 1) 
#' head(sheston91)
#' 
#' # lead
#'  sheston91$pop.L <- shift(x = pop, id = country, time = year, delta =  -1) 
#' head(sheston91)
#' 
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' 
#' @export
#'
shift <- function (x, id, time, delta = 1) 
{
  if (!is.integer(delta)) 
    delta <- as.integer(delta)
  if (length(id) != length(time)) 
    stop("The length of these two variables must be equal")
  if (any(duplicated(paste(id, time)))) 
    stop("The combination of id and time must be unique")
  if (any(data.frame(id, time) != data.frame(id[order(id,  time)], time[order(id, time)]))) {
    new.order <- order(id, time)
    x <- x[new.order]
    id <- id[new.order]
    time <- time[new.order]
  }
  x.shift <- x
  id.shift <- id
  time.shift <- time
  if (delta >= 1) {
    x.shift[length(id):(delta + 1)] <- x[(length(id) - delta):1]
    x.shift[1:delta] <- NA
    id.shift[length(id):(delta + 1)] <- id[(length(id) - 
                                              delta):1]
    time.shift[length(id):(delta + 1)] <- time[(length(id) -  delta):1]
  }
  else {
    x.shift[1:(length(id) + delta)] <- x[(-delta + 
                                            1):length(id)]
    x.shift[length(id):(length(id) + delta + 1)] <- NA
    id.shift[1:(length(id) + delta)] <- id[(-delta + 
                                              1):length(id)]
    time.shift[1:(length(id) + delta)] <- time[(-delta + 
                                                  1):length(id)]
  }
  x.shift[id != id.shift] <- NA
  if(exists("new.order")){
    x.shift <- x.shift[order(new.order)]
  }
  return(x.shift)
}
#' 
# shift <-
#function(x, delta=NA){
#  stopifnot(is.numeric(delta))
#  stopifnot(is.numeric(x))
#  
#  if (length(delta)>1)
#    return(sapply(delta,shift, x=x))
#  
#  output<-NULL
#  abs.delta=abs(delta)
#  if (delta > 0 )
#    output<-c(tail(x,-abs.delta),rep(NA,abs.delta))
#  else if (delta < 0 )
#    output<-c(rep(NA,abs.delta), head(x,-abs.delta))
#  else 
#    output <- x
#  return(output)
#}
