#' @title Method D'Hont
#' @description The function calculate the seats in parliament given the total number of seats and the votes for each party.
#' @param parties A vector containig the identification of parties or candidates accordingly to the election outcome.
#' @param votes A vector containing the total number of formal votes received by the parties/candidates.
#' @param seats An integer for the number of seats to be filled (the district magnitude).
#' @export
#' @examples
#' votes <- sample(1:10000, 5)
#' dHont(letters[1:5], votes, 10 )
#'
#' votes <- c(42201,38635,247736,170627,48236,117151,61379,35889,92321)
#' dHont(c("A","B","C","D","F","G","H","I","J"), votes, 26)
#'
dHont <- function(parties, votes, seats){
  tmp <- data.frame(
    parties = rep( parties, each = seats ),
    scores = as.vector(sapply( votes, function(x) x /
     1:seats ))
  )
  tmp <- tmp$parties[order(-tmp$scores)][1:seats]
  table(tmp)
}

