#' @encoding UTF-8
#' @title The Hamilton Method of Allocating Seats Proportionally
#'
#' @description Computes the Alexander Hamilton's apportionment method (1792), also known as Hare-Niemeyer method or as Vinton's method. The Hamilton method is a largest-remainder method which uses the Hare Quota.
#'
#' @param parties A vector containig parties labels or candidates accordingly to the \code{votes} vector order.
#' @param votes A vector containing the number of formal votes received by the parties/candidates.
#' @param seats An integer for the number of seats to be returned.
#' @details The Hamilton/Vinton Method sets the divisor as the proportion
#' of the total population per house seat. After each state's population
#' is divided by the divisor, the whole number of the quotient is kept
#' and the fraction dropped. This results in surplus house seats. Then,
#' the first surplus seat is assigned to the state with the largest
#' fraction after the original division. The next is assigned to the
#' state with the second-largest fraction and so on.
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}.
#'
#' @seealso \code{\link{dHondt}}, \code{\link{highestAverages}}, \code{\link{politicalDiversity}}.
#'
#' @docType methods
#' @importFrom utils head
#' @examples
#' votes <- sample(1:10000, 5)
#' parties <- sample(LETTERS, 5)
#' hamilton(parties, votes, 4)
#' @export
#' @rdname hamilton-methods
`hamilton` <- setClass("hamilton", slots = list(parties="character", votes = "integer", seats = "integer"))
NULL

setGeneric("hamilton", def=function(parties, votes, seats, ...){
  standardGeneric("hamilton")
})


#' @rdname hamilton-methods
setMethod(f="hamilton", definition=function(parties, votes, seats){
  .temp <- data.frame(
   parties = parties,
   scores = votes / sum(votes) * seats,
   perc = round(votes / sum(votes),3));
  integer <- with(.temp, floor(scores));
  fraction <- with(.temp, scores - integer);
  remainder <- seats - sum(integer);
  .temp[,2] <- integer;
  extra <- utils::head(order(fraction, decreasing=TRUE), remainder);
  .temp$scores[extra] <- (.temp$scores[extra] + 1);
  if(sum(.temp$scores) != seats) stop("Allocation error.");
  names(.temp) <-c("Parties", "Seats", "Shares");
  return(.temp);
})
NULL
