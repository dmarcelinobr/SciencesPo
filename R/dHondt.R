#' @title Method D'Hondt
#'
#' @description The function calculate the seats allotment in legislative house, given the total number of seats and the votes for each party based on the Victor D'Hondt's method (1878), which is mathematically equivalent to the method proposed by Thomas Jefferson few years before (1792).
#'
#' @param parties A vector containig parties labels or candidates accordingly to the \code{votes} vector order.
#' @param votes A vector containing the total number of formal votes received by the parties/candidates.
#' @param seats An integer for the number of seats to be filled (the district magnitude).
#'
#' @keywords Electoral
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}.
#'
#' @note Adapted from Carlos Bellosta's replies in the R-list.
#'
#' @export
#'
#' @examples
#' votes <- sample(1:10000, 5)
#' parties <- sample(letters, 5)
#' dHondt(parties, votes, 5 )
#'
#'
#' # Example: 2014 Brazilian election for the lower house in the state of Ceara.
#' # Coalitions leading by the following parties:
#'
#' results <- c(DEM=490205, PMDB=1151547, PRB=2449440,
#' PSB=48274, PSTU=54403, PTC=173151)
#'
#' dHondt(parties=names(results), votes=results, seats=19)
#'
#' # The next example is for the state legislative house of Ceara:
#'
#' votes <- c(187906, 326841, 132531, 981096, 2043217,15061,103679,109830, 213988, 67145, 278267)
#'
#' parties <- c("PCdoB", "PDT","PEN", "PMDB", "PRB","PSB","PSC", "PSTU", "PTdoB", "PTC", "PTN")
#'
#' dHondt(parties, votes , 42)
#'
`dHondt` <- function(parties, votes, seats){
  # creates a party score object
  .temp <- data.frame(
    parties = rep(parties, each = seats ),
    scores = as.vector(sapply( votes, function(x) x /
     1:seats ))
  );
  out <- with(.temp, (parties[order(-scores)][1:seats]))
   out <- data.frame(.freq(out)[,1:3]);
      names(out) <-c("Parties", "Frequency", "Percent");
   out <- out[ order(out[,2], decreasing = TRUE),]
     return(out)
}
NULL
