#' @title Method D'Hondt
#' @description The function calculate the seats in parliament given the total number of seats and the votes for each party.
#' @param parties A vector containig the identification of parties or candidates accordingly to the election outcome.
#' @param votes A vector containing the total number of formal votes received by the parties/candidates.
#' @param seats An integer for the number of seats to be filled (the district magnitude).
#' @export
#' @examples
#' votes <- sample(1:10000, 5)
#' dHondt(letters[1:5], votes, 5 )
#'
#' #Example: 2014 Brazilian election for the lower house in the state of Ceara:
#' votes <- c(490205, 1151547, 2449440, 48274, 54403, 173151)
#'
#' # Coalitions leading by the following parties:
#' parties <- c("DEM","PMDB","PRB","PSB", "PSTU","PTC")
#'
#' dHondt(parties, votes, seats=19)
#'
dHondt <- function(parties, votes, seats){
  tmp <- data.frame(
    parties = rep( parties, each = seats ),
    scores = as.vector(sapply( votes, function(x) x /
     1:seats ))
  )
  tmp <- tmp$parties[order(-tmp$scores)][1:seats]
  table(tmp)
}

