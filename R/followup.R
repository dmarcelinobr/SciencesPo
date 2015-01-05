#' @title Mark Followup Interviews by id and time
#'
#' @param id the identification of the subject.
#' @param time the date followup contact. 
#' 
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @export
followup <- function (id, time) {
if(length(id) !=length(time)) stop("The length of these two variables must be equal")
if(any(duplicated(paste(id,time)))) stop("The combination of id and time must be unique")
original.order <- 1:length(id)
if(any(data.frame(id, time) != data.frame(id[order(id, time)], time[order(id,time)]))){
  new.order <- original.order[order(id,time)]
  id <- id[order(id,time)]
  time <- time[order(id,time)]
}
list1 <- rle(as.vector(id))
unlist(sapply(X=list1$lengths, FUN=function(x) 1:x, simplify=FALSE)) -> visit
visit[order(original.order)]
}