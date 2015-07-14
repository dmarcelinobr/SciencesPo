#' @title Method D'Hondt
#' @description The function calculate the seats allotment in legislative house, given the total number of seats and the votes for each party based on the Victor D'Hondt's method (1878), which is mathematically equivalent to the method proposed by Thomas Jefferson few years before (1792).
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
#' # The next example is for the state legislative house of Ceara:
#'
#' votes <- c(187906, 326841,132531, 981096, 2043217,15061,103679,109830, 213988, 67145, 278267)
#'
#' parties <- c("PC do B", "PDT","PEN", "PMDB", "PRB","PSB","PSC", "PSTU", "PT do B", "PTC", "PTN")
#'
#' dHondt(parties, votes , 42)
#'
dHondt <- function(parties, votes, seats){
  .temp <- data.frame(
    parties = rep( parties, each = seats ),
    scores = as.vector(sapply( votes, function(x) x /
     1:seats ))
  )
  .dHondt <- .temp$parties[order(-.temp$scores)][1:seats]
  table(.dHondt)
}

