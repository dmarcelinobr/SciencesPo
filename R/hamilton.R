#' @title The Hamilton method of apportionment
#'
#' @description Compute the Alexander Hamilton's apportionment method (1971).
#' @param votes A vector containing the number of formal votes received by the parties/candidates.
#' @param seats An integer for the number of seats to be returned.
#' @details The Hamilton/Vinton Method sets the divisor as the proportion
#' of the total population per house seat. After each state's population
#' is divided by the divisor, the whole number of the quotient is kept
#' and the fraction dropped. This results in surplus house seats. Then,
#'  the first surplus seat is assigned to the state with the largest
#'  fraction after the original division. The next is assigned to the
#'  state with the second-largest fraction and so on.
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}.
#' @examples
#' votes <- sample(1:10000, 5)
#' hamilton( votes, 8)
#' @export
hamilton <- function(votes, seats) {

  allot <- votes / sum(votes) * seats

  integer <- floor(allot)
  fraction <- allot - integer

  n <- seats - sum(integer)
  extra <- head(order(fraction, decreasing=TRUE), n)

  allot <- integer
  allot[extra] <- allot[extra] + 1

  if(sum(allot) != seats) stop("Allocation error.")

  return(allot)
}
NULL


quotient <- function(votes, total=120, start, f) {

  if(missing(f)) f <- function(s) 2*s + 1

  if(missing(start)) start <- hamilton(votes, total)
  alloc <- start

  stopifnot(sum(alloc)==total)
  stopifnot(length(alloc)==length(votes))

  for(it in c(0, seq_len(total))) {

    # Q0 are the quotients justifying
    # the current seat allocations
    Q0 <- votes / f(alloc - 1)
    Q0[votes == 0] <- 0
    Q0[alloc == 0] <- Inf

    # Q1 are the quotients for additional seats
    Q1 <- votes / f(alloc)

    # All of the Q0 should be larger than all of the Q1,
    # otherwise parties with larger Q1 quotients need more seats.
    if(min(Q0) > max(Q1)) break

    # The correct allocation should be guaranteed after
    # this many adjustments. Final pass through the loop
    # is only to test that the allocation is correct.
    stopifnot(it < total)

    m <- median(c(Q0, Q1))

    inc <- (Q1 >= m)
    dec <- (Q0 < m)

    stopifnot(sum(inc)==sum(dec)) # shouldn't be possible?

    alloc[inc] <- alloc[inc] + 1
    alloc[dec] <- alloc[dec] - 1
  }

  stopifnot(sum(alloc)==total) # don't allocate wrong number

  attr(alloc, "iterations") <- it

  return(alloc)
}
SainteLague <- Webster <- function(...) quotient(..., f=function(s) 2*s + 1)

mSainteLague <- function(...) quotient(..., f=function(s) ifelse(s==0, 1.4, 2*s + 1))

#'# dHondt <- Jefferson <- function(...) quotient(..., f=function(s) s + 1)

Danish <- function(...) quotient(..., f=function(s) 3*s + 1)

Imperiali <- function(...) quotient(..., f=function(s) s/2 + 1)
