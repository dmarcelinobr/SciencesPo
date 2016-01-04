#' @encoding UTF-8
#' @title The D'Hondt Method of Allocating Seats Proportionally
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
#' @seealso \code{\link{highestAverages}}, \code{\link{hamilton}}, \code{\link{politicalDiversity}}.
#'
#' @note Adapted from Carlos Bellosta's replies in the R-list.
#'
#' @examples
#' # Example: 2014 Brazilian election for the lower house in the state of Ceara.
#' # Coalitions leading by the following parties:
#'
#' results <- c(DEM=490205, PMDB=1151547, PRB=2449440,
#' PSB=48274, PSTU=54403, PTC=173151)
#'
#' dHondt(parties=names(results), votes=results, seats=19)
#'
#' # The next example is for the state legislative house of Ceara (2014):
#'
#' votes <- c(187906, 326841, 132531, 981096, 2043217,15061,103679,109830, 213988, 67145, 278267)
#'
#' parties <- c("PCdoB", "PDT","PEN", "PMDB", "PRB","PSB","PSC", "PSTU", "PTdoB", "PTC", "PTN")
#'
#' dHondt(parties, votes , seats=42)
#'
#' @docType methods
#' @importFrom utils head
#' @rdname dHondt-methods
#' @export
`dHondt` <- setClass("dHondt", slots = list(parties="character", votes = "integer", seats = "integer"))
NULL

setGeneric("dHondt", def=function(parties, votes, seats, ...){
  standardGeneric("dHondt")
})


#' @rdname dHondt-methods
setMethod(f="dHondt", definition=function(parties, votes, seats){
  # creates a party score object
  .temp <- data.frame(
    parties = rep(parties, each = seats ),
    scores = as.vector(sapply( votes, function(x) x /
     1:seats ))
  );
  out <- with(.temp, (parties[order(-scores)][1:seats]))
   out <- data.frame(.freq(out)[,1:3]);
      names(out) <-c("Parties", "Seats", "Shares");
   out <- out[ order(out[,2], decreasing = TRUE),]
     return(out)
})
NULL





#' @encoding latin1
#' @title The Highest Averages Method of Allocating Seats Proportionally
#'
#' @description Computes the highest averages method for a variety of formulas to allocate seats proportionally for voting systems with representative assemblies.
#' @param parties A character vector for parties labels or candidates accordingly to the \code{votes} order.
#' @param votes A numeric vector for the number of formal votes received by each party or candidate.
#' @param seats The number of seats to be filled (scalar or vector).
#' @param method A character name for the method to be used. See details.
#'
#' @return A \code{data.frame}.
#' @keywords Electoral
#'
#' @details The following methods are available:
#' \itemize{
#' \item {"dt"}{d'Hondt method}
#' \item {"sl"}{Sainte-Laguë method}
#' \item {"msl"}{Modified Sainte-Laguë method}
#' \item {"danish"}{Danish method}
#' \item {"imperiali"}{Imperiali (not to be confused with the Imperiali quota which is a Largest remainder method)}
#' \item {"hh"}{Huntington-Hill method}
#' }
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}.
#' @seealso \code{\link{dHondt}}, \code{\link{hamilton}}, \code{\link{politicalDiversity}}.
#'
#' @examples
#' # Results for the state legislative house of Ceara (2014):
#'
#' votes <- c(187906, 326841, 132531, 981096, 2043217,15061,103679,109830, 213988, 67145, 278267)
#'
#' parties <- c("PCdoB", "PDT","PEN", "PMDB", "PRB","PSB","PSC", "PSTU", "PTdoB", "PTC", "PTN")
#'
#' highestAverages(parties, votes, seats = 42, method="dt")
#'
#' # Let's create a typical election result data.frame with the following parties and votes to return 10 seats:
#'
#' my_election <- data.frame(
#' party=c("Yellow", "White", "Red", "Green", "Blue", "Pink"),
#' votes=c(47000, 16000,	15900,	12000,	6000,	3100))
#'
#' highestAverages(my_election$party,
#' my_election$votes,
#' seats = 10,
#' method="dt")
#'
#' # How this compares to the Sainte-Laguë Method
#'
#' highestAverages(my_election$party,
#' my_election$votes,
#' seats = 10,
#' method="sl")
#'
#' @docType methods
#' @importFrom utils head
#' @rdname highestAverages-methods
#' @export
`highestAverages` <-setClass("highestAverages",
                             slots = list(parties="character", votes = "integer", seats = "integer", method = "integer"))
NULL

setGeneric("highestAverages", def=function(parties, votes, seats, ...){
  standardGeneric("highestAverages")
})

#' @rdname highestAverages-methods
setMethod(f="highestAverages", definition=function(parties, votes, seats, method){
#Define Quotient

  switch(method,
         dt = { #d'Hondt
           wari.vec <- seq(from = 1, by = 1, length.out = seats)
           method.name <- c("d'Hondt")
         },
         sl = { #Sainte-Laguë
           wari.vec <- seq(from = 1, by = 2, length.out = seats)
           method.name <- c("Sainte-Laguë")
         },
         msl = { #Modified Sainte-Laguë
           wari.vec <- c(1.4, seq(from = 3, by = 2, length.out = seats-1))
           method.name <- c("Modified Sainte-Laguë")
         },
         danish = { #Danish
           wari.vec <- c(2, seq(from = 3, by = 1, length.out = seats-1))
           method.name <- c("Danish")
         },
         imperiali = { #Imperiali
           wari.vec <- c(2, seq(from = 3, by = 1, length.out = seats-1))
           method.name <- c("Imperiali")
         },
         hh = { #Huntington-Hill
           wari.vec0 <- seq(from = 1, by = 1, length.out = seats)
           wari.vec <- sqrt(wari.vec0 * (wari.vec0 - 1))
           method.name <- c("Hungtinton-Hill")
         }
)

.temp <- data.frame(
        parties = rep(parties, each = seats ),
        scores = as.vector(sapply( votes, function(x) x /
                                     wari.vec ))
      );

out <- with(.temp, (parties[order(-scores)][1:seats]))
out <- data.frame(.freq(out)[,1:3]);
names(out) <-c("Parties", "Seats", "Shares");
HA.out <- out[ order(out[,2], decreasing = TRUE),]
# Measures
ENP_final <- 1/sum((HA.out$Seats/sum(HA.out$Seats))^2)
# G.index <- sqrt(0.5 * sum((((raw.votes/sum(raw.votes))*100) - ((result.vec/sum(result.vec))*100))^2))

cat("Method:", method.name, "\n")
cat(paste("ENP(Final):", round(ENP_final, 2)), "\n \n")
#cat(paste("Gallagher Index:", round(G.index, 3)), "\n")
return(HA.out)
})
NULL
