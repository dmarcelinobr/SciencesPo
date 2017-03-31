#' @encoding latin1
#' @title Highest Averages Methods of Allocating Seats Proportionally
#'
#' @description Computes the highest averages method for a variety of formulas of allocating seats proportionally.
#' @param parties A character vector for parties labels or candidates in the same order as \code{votes}. If \code{NULL}, alphabet will be assigned.
#' @param votes a numeric vector for the number of formal votes received by each party or candidate.
#' @param seats the number of seats to be filled (scalar or vector).
#' @param method a character name for the method to be used. See details.
#' @param threshold a numeric value between (0~1). Default is set to 0.
#' @param \dots Additional arguements (currently ignored)
#'
#' @return A \code{data.frame} of length \code{parties} containing apportioned integers (seats) summing to \code{seats}.
#' @keywords Electoral
#'
#' @details The following methods are available:
#' \itemize{
#' \item {"dh"}{d'Hondt method}
#' \item {"sl"}{Sainte-Lague method}
#' \item {"msl"}{Modified Sainte-Lague method}
#' \item {"danish"}{Danish modified Sainte-Lague method}
#' \item {"hsl"}{Hungarian modified Sainte-Lague method}
#' \item {"imperiali"}{The Italian Imperiali (not to be confused with the Imperiali Quota, which is a Largest remainder method)}
#' \item {"hh"}{Huntington-Hill method}
#' \item {"wb"}{Webster's method}
#' \item {"jef"}{Jefferson's method}
#' \item {"ad"}{Adams's method}
#' \item {"hb"}{Hagenbach-Bischoff method}
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
#'
#' @seealso \code{\link{largestRemainders}}, \code{\link{proportionality}}, \code{\link{politicalDiversity}}. For more details see the \emph{Indices} vignette: \code{vignette('Indices', package = 'SciencesPo')}.
#'
#' @examples
#' # Results for the state legislative house of Ceara (2014):
#' votes <- c(187906, 326841, 132531, 981096, 2043217, 15061, 103679,109830, 213988, 67145, 278267)
#'
#' parties <- c("PCdoB", "PDT", "PEN", "PMDB", "PRB", "PSB", "PSC", "PSTU", "PTdoB", "PTC", "PTN")
#'
#' highestAverages(parties, votes, seats = 42, method = "dh")
#'
#' # Let's create a data.frame with typical election results
#' # with the following parties and votes to return 10 seats:
#'
#' my_election_data <- data.frame(
#' party=c("Yellow", "White", "Red", "Green", "Blue", "Pink"),
#' votes=c(47000, 16000,	15900,	12000,	6000,	3100))
#'
#' highestAverages(my_election_data$party,
#' my_election_data$votes,
#' seats = 10,
#' method="dh")
#'
#' # How this compares to the Sainte-Lague Method
#'
#'(dat= highestAverages(my_election_data$party,
#' my_election_data$votes,
#' seats = 10,
#' method="sl"))
#'
#' # Plot it
#' # Barplot(data=dat, "Party", "Seats") +
#' # theme_fte()
#'
#' @rdname highestAverages
#' @export
`highestAverages` <-
  function(parties = NULL,
           votes = NULL,
           seats = NULL,
           method = c("dh",
                      "sl",
                      "msl",
                      "danish",
                      "hsl",
                      "hh",
                      "imperiali",
                      "wb",
                      "jef",
                      "ad",
                      "hb"),
           threshold = 0,
           ...)
UseMethod("highestAverages")



#' @export
#' @rdname highestAverages
`highestAverages.default` <-
  function(parties = NULL,
           votes = NULL,
           seats = NULL,
           method = c("dh",
                      "sl",
                      "msl",
                      "danish",
                      "hsl",
                      "hh",
                      "imperiali",
                      "wb",
                      "jef",
                      "ad",
                      "hb"),
           threshold = 0,
           ...) {
    # Modified :
    # v0.0 2012-07-12
    # v0.0 2013-11-21
    # v0.2 2014-10-02
    # v0.3 2016-01-13
    # v0.3 2016-05-15
    # local vars for using later
    .ratio <- votes / sum(votes)
    .votes <- ifelse(.ratio < threshold, 0, votes)

    # To deal with  NULL party labels
    if (is.null(parties)) {
      parties <- replicate(length(votes),
                           paste(sample(LETTERS, 3,
                                        replace = TRUE), collapse = ""))
    }

    # Define Quotient
    switch(
      method,
      dh = {
        #d'Hondt
        divisor.vec <- seq(from = 1,
                           by = 1,
                           length.out = seats)
        method.name <- c("d'Hondt")
      },
      sl = {
        #Sainte-Lague
        divisor.vec <- seq(from = 1,
                           by = 2,
                           length.out = seats)
        method.name <- c("Sainte-Lagu\u00EB")
      },
      msl = {
        #Modified Sainte-Lague
        divisor.vec <-
          c(1.4, seq(
            from = 3,
            by = 2,
            length.out = seats - 1
          ))
        method.name <- c("Modified Sainte-Lagu\u00EB")
      },
      danish = {
        #Danish
        divisor.vec <-
          c(1, seq(
            from = 4,
            by = 3,
            length.out = seats - 1
          ))
        method.name <- c("Danish Sainte-Lagu\u00EB")
      },
      hsl = {
        #Hungarian
        divisor.vec <-
          c(1.5, seq(
            from = 3,
            by = 2,
            length.out = seats - 1
          ))
        method.name <- c("Hungarian Sainte-Lagu\u00EB")
      },
      imperiali = {
        #Imperiali
        divisor.vec <-
          c(1, seq(
            from = 1.5,
            by = .5,
            length.out = seats - 1
          ))
        method.name <- c("Imperiali")
      },
      hh = {
        #Huntington-Hill Equal Proportions Method
        divisor.vec0 <- seq(from = 1,
                            by = 1,
                            length.out = seats)
        divisor.vec <- sqrt(divisor.vec0 * (divisor.vec0 - 1))
        method.name <- c("Hungtinton-Hill")
      },
      wb = {
        #Webster Major Fractions Method
        divisor.vec0 <- seq(from = 1,
                            by = 2,
                            length.out = seats)
        divisor.vec <- (divisor.vec0 + (divisor.vec0 - 1)) / 2
        method.name <- c("Webster")
      },
      jef = {
        #Jefferson Greatest Divisors or Hagenbach-Bischoff Method
        divisor.vec <- seq(from = 1,
                           by = 1,
                           length.out = seats)
        method.name <- c("Jefferson")
      },
      ad = {
        #Adam's Method Smallest Devisors
        divisor.vec <-
          c(0, seq(
            from = 1,
            by = 1,
            length.out = seats - 1
          ))
        method.name <- c("Adam's Method")
      },
      hb = {
        #Hagenbach-Bischoff Method
        divisor.vec <- seq(from = 1,
                           by = 1,
                           length.out = seats)
        method.name <- c("Hagenbach-Bischoff")
      }
    )

    # ratio = as.vector(sapply(votes, function(x) x /
    # sum(votes)))
.temp <- data.frame(parties = rep(parties, each = seats),
                        scores = as.vector(sapply(.votes, function(x)
                          x / divisor.vec)))

out <- with(.temp, (parties[order(-scores)][1:seats]))
output <- freq(out, digits = 3, perc = TRUE)

    # Political diversity indices
    ENP_votes <- 1 / sum(.ratio ^ 2)
    ENP_seats <- 1 / sum((output$Freq / sum(output$Freq)) ^ 2)
    LSq_index <-
      sqrt(0.5 * sum((((
        votes / sum(votes)
      ) * 100) - ((output$Freq / sum(output$Freq)) * 100
      )) ^ 2))

    .shorten <- function(x, n)
      cat("Divisors:", x[1:n], "...", "\n")

    cat("Method:", method.name, "\n")
    .shorten(round(divisor.vec, 2), 4)
    cat(paste("ENP:", round(ENP_votes, 2), "(After):", round(ENP_seats, 2)), "\n")
    cat(paste("Gallagher Index: ", round(LSq_index, 2)), "\n \n")
    # names(output) <-c("Party", "Seats", "Seats(\u0025)");
    names(output) <- c("Party", "Seats", "\u0025 Seats")

    # output <- output[ order(output[,2], decreasing = TRUE),]
    class(output) <- c("SciencesPo", class(output))
    attr(output, "scpo.type") <- "Standard"
    return(output)
  }
NULL
