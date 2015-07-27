#' @title Seat allocation and its ordering
#'
#' @description Compute seat allocation and its ordering for several values of seats and divisor methods with the same quotiens table.
#' @param parties A vector containig the identification of parties or candidates accordingly to the election outcome.
#' @param votes A vector containing the total number of formal votes received by the parties/candidates.
#' @param seats A vector for the number of seats to be filled (the district magnitude).
#' @param method An integer for the method of allocation: method=2 Sainte-Lague; method=1 D'Hondt
#' @param hurdle The percentage of votes that must be reached to get seats in parliament.
#' @examples
#' # Allocation example:
#' votes <- sample(1:10000, 5)
#' allocation(letters[1:5], votes, seats=8, method=1)
#' allocation(letters[1:5], votes, seats=c(3,5,8), method=2:1, hurdle=0.5)

#'
#' @export
#'
`allocation` <- function(parties, votes, seats, method, hurdle=0){
  votes=votes*(votes>=(hurdle*sum(votes)))
  max_method <- max(method)
  max_seats <- max(seats)
  nquotients=(1+max_method*(max_seats-1))
  #table with all the quotients needed
  quotienstable <- data.frame(
    parties    = rep(parties, each = nquotients),
    quotients  = as.vector(sapply(votes, function(x)
      x/seq(from=1, to=nquotients) )),
    votesrep   = rep(votes, each = nquotients)
  )
  SeatsList=list()
  filteredtable =list()
  for (j in 1:length(method)) {
    #select from the table of all quotiens the ones whose divisors
    #belong to the current sequence, given 'seats' and 'method'
    select <- rep(seq(from=1, to=1+method[j]*(max_seats-1), by=method[j]),length(parties))+
      rep(seq(from=0,to=nquotients*length(parties)-1,by=nquotients),each=max_seats)
    filteredtable [[j]] <- quotienstable$parties[select][order(-quotienstable$quotients[select], -quotienstable$votesrep[select])]
    SeatsList[[j]]<-list()
    #a vector of values for seats if you want to compute the partial sums
    for (i in 1:length(seats)){
      SeatsList[[j]][[i]]= table(filteredtable[[j]][1:seats[i]])
    }
    names(SeatsList[[j]])[1:length(seats)]<-do.call("paste0",as.data.frame(cbind("divisor method ",method[j]," for ", seats," seats")))
    SeatsList[[j]][[length(seats)+1]] = as.matrix(filteredtable[[j]][1:max_seats]) #ordering

    names(SeatsList[[j]])[length(seats)+1]<-paste0("ordering for divisor method ", method[j]," for ", max_seats," seats")

  }
  return(SeatsList);
}
NULL
