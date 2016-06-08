
#' @encoding latin1
#' @title Largest Remainders Methods of Allocating Seats Proportionally
#'
#' @description Computes the largest remainders method for a variety of formulas of allocating seats proportionally.
#' @param parties A character vector for parties labels or candidates in the order as \code{votes}. If \code{NULL}, a random combination of letters will be assigned.
#' @param votes A numeric vector for the number of formal votes received by each party or candidate.
#' @param seats The number of seats to be filled (scalar or vector).
#' @param method A character name for the method to be used. See details.
#' @param threshold A numeric value between (0~1). Default is set to 0.
#' @param \dots Additional arguements (currently ignored)
#'
#' @return A \code{data.frame} of length \code{parties} containing apportioned integers (seats) summing to \code{seats}.
#' @keywords Electoral
#'
#' @details The following methods are available:
#' \itemize{
#' \item {"droop"}{Droop quota method}
#' \item {"hare"}{Hare method}
#' \item {"hagb"}{Hagenbach-Bischoff}
#' \item {"imperiali"}{Imperiali quota (do not confuse with the Italian Imperiali, which is a highest averages method)}
#' \item {"imperiali.adj"}{Reinforced or adjusted Imperiali quota}
#' }
#'
#' @references
#' Gallagher, Michael (1992). "Comparing Proportional Representation
#' Electoral Systems: Quotas, Thresholds, Paradoxes and Majorities".
#' \emph{British Journal of Political Science}, 22, 4, 469-496.
#'
#'  Lijphart, Arend (1994). \emph{Electoral Systems and Party Systems: A Study of Twenty-Seven Democracies, 1945-1990}. Oxford University Press.
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}.
#' @seealso  \code{\link{HighestAverages}}, \code{\link{Proportionality}}, \code{\link{PoliticalDiversity}}. For more details see the \emph{Indices} vignette: \code{vignette('Indices', package = 'SciencesPo')}.
#'
#' @examples
#' # Let's create a data.frame with typical election results
#' # with the following parties and votes to return 10 seats:
#'
#' my_election_data <- data.frame(
#' party=c("Yellow", "White", "Red", "Green", "Blue", "Pink"),
#' votes=c(47000, 16000,	15900,	12000,	6000,	3100))
#'
#' LargestRemainders(my_election_data$party,
#' my_election_data$votes, seats = 10,  method="droop")
#'
#' with(my_election_data, LargestRemainders(party,
#' votes, seats = 10,  method="hare"))
#'
#' @rdname LargestRemainders
#' @export
`LargestRemainders` <- function(parties=NULL, votes=NULL, seats=NULL, method=c("hare", "droop", "hagb", "imperiali", "imperiali.adj"), threshold=0, ...) UseMethod("LargestRemainders")



#' @export
#' @rdname LargestRemainders
`LargestRemainders.default` <- function(parties=NULL, votes=NULL, seats=NULL, method=c("hare", "droop", "hagb", "imperiali", "imperiali.adj"), threshold=0, ...){
  # Modified :
  # v0.0 2013-11-21
  # v0.1 2014-10-02
  # v0.2 2016-01-13
  # v0.2 2016-05-15
  # local vars for using later
  .ratio <- votes/sum(votes)
  .votes <- ifelse(.ratio < threshold, 0, votes)

  # To deal with  NULL party labels
  if (is.null(parties)){
    parties <- replicate(length(votes),
                         paste(sample(LETTERS, 3,
                                      replace=TRUE), collapse=""))
  }

  # Define Quotient
  switch(method,
         hare = { # Hare
           divisor.vec <- (sum(.votes)/seats)
           method.name <- c("Hare")
         },
         droop = { #Droop
           divisor.vec <- (1 + (sum(.votes)/(seats+1)))
           method.name <- c("Droop")
         },
         hagb = { #Hagenbach-Bischoff
           divisor.vec <- (sum(.votes)/(seats+1))
           method.name <- c("Hagenbach-Bischoff")
         },
         imperiali = { #Imperiali quota
           divisor.vec <- (sum(.votes)/(seats + 2))
           method.name <- c("Imperiali quota")
         },
         imperiali.adj = { #Reinforced Imperiali quota
           divisor.vec <- (sum(.votes)/(seats + 3))
           method.name <- c("Reinforced Imperiali quota")
         })

  seat.distribution <- .votes%/%divisor.vec
  remainder <- seats - sum(seat.distribution)
  .temp <- data.frame(
    party = rep(parties, each = 1),
    scores = as.vector(sapply(.votes, function(x) x /
                                divisor.vec ) - seat.distribution)
  );

  .temp <- .temp[order(as.double(.temp$scores), decreasing = TRUE),]

  rownames(.temp) <- c(1:nrow(.temp))
  .temp <- .temp[1:remainder,]

  out <- data.frame(party = rep(parties, each = 1),
                    seat = seat.distribution)

  if(as.integer(remainder) == 0){
  }else if(as.integer(remainder) == 1){
    out[as.integer(.temp[1,]$party), 2] <- out[as.integer(.temp[1,]$party), 2] + 1
  }else{
    for(i in 1:remainder){
      out[as.integer(.temp[i,]$party), 2] <- out[as.integer(.temp[i,]$party), 2] + 1
    }
  }

  output <- SciencesPo::freq(out, digits = 3, perc=TRUE);
  # Political diversity indices
  ENP_votes <- 1/sum(.ratio^2)
  ENP_seats <- 1/sum((output$Freq/sum(output$Freq))^2)
  LSq_index <- sqrt(0.5*sum((((votes/sum(votes))*100) - ((output$Freq/sum(output$Freq))*100))^2))

.shorten <- function(x, n)
    cat("Divisors:", x[1:n], "...", "\n")

  cat("Method:", method.name, "\n")
  .shorten(round(divisor.vec, 2), 4)
  cat(paste("ENP:",round(ENP_votes,2),"(After):",round(ENP_seats,2)),"\n")
  cat(paste("Gallagher Index: ", round(LSq_index, 2)), "\n \n")
  # names(output) <-c("Party", "Seats", "Seats(\u0025)");
  names(output) <-c("Party", "Seats", "\u0025 Seats");
  class(output) <- c("SciencesPo", class(output))
  attr(output, "scpo.type") <- "Standard"
  return(output)
}
NULL
