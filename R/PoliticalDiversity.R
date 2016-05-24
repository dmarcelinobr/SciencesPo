#' @encoding UTF-8
#' @title Political Diversity Indices
#'
#' @description Computes political diversity indices or fragmentation/concetration
#'  measures like the effective number of parties for an electoral unity or across
#'  unities. The very intuition of these coefficients is to counting parties while
#'  weighting them by their relative political--or electoral strength.
#'
#' @param x A data.frame, a matrix, or a vector containing values for the number
#' of votes or seats each party received.
#' @param index The type of index desired, one of "laakso/taagepera", "golosov",
#' "herfindahl", "gini", "shannon", "simpson", "invsimpson".
#' @param margin The margin for which the index is computed.
#' @param base The logarithm base used in some indices, such as the "shannon" index.
#'
#' @details Very often, political analysts say things like \sQuote{two-party system} and
#'  \sQuote{multi-party system} to refer to a particular kind of political party system.
#' However, these terms alone does not tell exactly how fragmented--or concentrated a
#' party system actually is. For instance, after the 2010 general election, 22 parties
#' obtained representation in the Lower Chamber in Brazil. Nonetheless, among these 22
#' parties, nine parties together returned only 28 MPs. Thus, an index to assess the
#' weigh or the \bold{Effective Number of Parties} is important and helps to go
#' beyond the simple count of parties in a legislative branch.
#' A widely accepted algorithm was proposed by M. Laakso and R. Taagepera:
#' \deqn{N = \frac{1}{\sum p_i^2}}{N = 1/ \sum p_i^2}, where \bold{N} denotes the
#' effective number of parties and \bold{p_i} denotes the \eqn{it^h} party's fraction
#' of the seats.
#'
#' In fact, this formula may be used to compute the vote share for each party. This
#' formula is the reciprocal of a well-known concentration index
#' (\bold{the Herfindahl-Hirschman index}) used in economics to study the degree
#' to which ownership of firms in an industry is concentrated. Laakso and Taagepera
#' correctly saw that the effective number of parties is simply an instance of the
#' inverse measurement problem to that one. This index makes rough but fairly
#' reliable international comparisons of party systems possible.
#'
#' \bold{The Inverse Simpson index},
#' \deqn{ 1/ \lambda = {1 \over\sum_{i=1}^R p_i^2} = {}^2D}
#' Where \eqn{\lambda} equals the probability that two types taken at random from
#' the dataset (with replacement) represent the same type. This simply equals true
#' fragmentation of order 2, i.e. the effective number of parties that is obtained
#' when the weighted arithmetic mean is used to quantify average proportional
#' diversity of political parties in the election of interest.
#' Another measure is the \bold{Least squares index (lsq)}, which measures the
#' disproportionality produced by the election. Specifically, by the disparity
#' between the distribution of votes and seats allocation.
#'
#' Recently, Grigorii Golosov proposed a new method for computing the effective
#' number of parties  in which both larger and smaller parties are not attributed unrealistic scores as those resulted by using the Laakso/Taagepera index.I will
#' call this as (\bold{Golosov}) and is given by the following
#' formula: \deqn{N = \sum_{i=1}^{n}\frac{p_{i}}{p_{i}+p_{i}^{2}-p_{i}^{2}}}
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}.
#'
#' @references Gallagher, Michael and Paul Mitchell (2005) \emph{The Politics of Electoral Systems.} Oxford University Press.
#'
#' Golosov, Grigorii (2010) The Effective Number of Parties: A New Approach, \emph{Party Politics,} \bold{16:} 171-192.
#'
#' Laakso, Markku and Rein Taagepera (1979) Effective Number of Parties: A Measure with Application to West Europe, \emph{Comparative Political Studies,} \bold{12:} 3-27.
#'
#' Nicolau, Jairo (2008) \emph{Sistemas Eleitorais.} Rio de Janeiro, FGV.
#'
#' Taagepera, Rein and Matthew S. Shugart (1989) \emph{Seats and Votes: The Effects and Determinants of Electoral Systems.} New Haven: Yale University Press.
#'
#' @keywords Exploratory, Electoral
#' @examples
#' # Here are some examples, help yourself:
#' # The wikipedia examples
#'
#' A <- c(.75,.25);
#' B <- c(.75,.10,rep(0.01,15))
#' C <- c(.55,.45);
#'
#' # The index by "laakso/taagepera" is the default
#' PoliticalDiversity(A)
#' PoliticalDiversity(B)
#'
#' # Using the method proposed by Golosov gives:
#' PoliticalDiversity(B, index="golosov")
#' PoliticalDiversity(C, index="golosov")
#'
#' # The 1980 presidential election in the US (vote share):
#' US1980 <- c("Democratic"=0.410, "Republican"=0.507,
#' "Independent"=0.066, "Libertarian"=0.011, "Citizens"=0.003,
#' "Others"=0.003)
#'
#' PoliticalDiversity(US1980)
#'
#' # 2010 Brazilian legislative election
#'
#' votes_2010 = c("PT"=13813587, "PMDB"=11692384, "PSDB"=9421347,
#' "DEM"=6932420, "PR"=7050274, "PP"=5987670, "PSB"=6553345,
#' "PDT"=4478736, "PTB"=3808646, "PSC"=2981714, "PV"=2886633,
#' "PC do B"=2545279, "PPS"=2376475, "PRB"=1659973, "PMN"=1026220,
#' "PT do B"=605768, "PSOL"=968475, "PHS"=719611, "PRTB"=283047,
#' "PRP"=232530, "PSL"=457490,"PTC"=563145)
#'
#' seats_2010 = c("PT"=88, "PMDB"=79, "PSDB"=53, "DEM"=43,
#' "PR"=41, "PP"=41, "PSB"=34, "PDT"=28, "PTB"=21, "PSC"=17,
#' "PV"=15, "PC do B"=15, "PPS"=12, "PRB"=8, "PMN"=4, "PT do B"=3,
#'  "PSOL"=3, "PHS"=2, "PRTB"=2, "PRP"=2, "PSL"=1,"PTC"=1)
#'
#' PoliticalDiversity(seats_2010)
#'
#' PoliticalDiversity(seats_2010, index= "golosov")
#'
#' @export
#' @rdname PoliticalDiversity
`PoliticalDiversity`<- function(x, index = "laakso/taagepera", margin=1, base = exp(1))
  UseMethod("PoliticalDiversity")

#' @export
#' @rdname PoliticalDiversity
`PoliticalDiversity.default`<- function(x, index = "laakso/taagepera", margin=1, base = exp(1)){
  x <- drop(as.matrix(x))
  index <- .Match(arg = index, choices = c("laakso/taagepera", "golosov", "lsq", "enc",  "enp", "herfindahl", "gini", "simpson", "invsimpson","shannon") )
  if (length(dim(x)) > 1) {
    total <- apply(x, margin, sum)
    x <- sweep(x, margin, total, "/")
  }
  else {
    x <- x/sum(x)
  }
  if (index == "shannon")
    x <- -x * log(x, base)
  else if (index=="golosov")
    x <- sum((x)/((x)+((x[1])^2)-((x)^2)))
  else x <- x * x
  if (length(dim(x)) > 1)
    idx <- apply(x, margin, sum, na.rm = TRUE)
  else idx <- sum(x, na.rm = TRUE)
  if (index == "simpson"||index == "herfindahl")
    idx <- 1 - idx
  else if (index == "laakso/taagepera" || index == "invsimpson"||  index == "enc" || index == "enp")
    idx <- 1/idx
  return(round(idx, 3))
}### end -- politicalDiversity function
NULL




#' @encoding UTF-8
#' @title The Hamilton Method of Allocating Seats Proportionally
#'
#' @description Computes the Alexander Hamilton's apportionment method (1792), also known as Hare-Niemeyer method or as Vinton's method. The Hamilton method is a largest-remainder method which uses the Hare Quota.
#'
#' @param parties A vector containig parties labels or candidates in the same order of \code{votes}.
#' @param votes A vector with the formal votes received by the parties/candidates.
#' @param seats An integer for the number of seats to be returned.
#' @param \dots Additional arguements (currently ignored)
#' @return A \code{data.frame} of length \code{parties} containing apportioned integers (seats) summing to \code{seats}.
#' @details The Hamilton/Vinton Method sets the divisor as the
#' proportion of the total population per house seat.
#' After each state's population is divided by the divisor,
#' the whole number of the quotient is kept and the fraction
#' dropped resulting in surplus house seats. Then, the first
#' surplus seat is assigned to the state with the largest
#' fraction after the original division. The next is assigned to
#' the state with the second-largest fraction and so on.
#' @references
#'  Lijphart, Arend (1994). \emph{Electoral Systems and Party Systems: A Study of Twenty-Seven Democracies, 1945-1990}. Oxford University Press.
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}.
#'
#' @seealso \code{\link{dHondt}}, \code{\link{HighestAverages}}, \code{\link{LargestRemainders}}, \code{\link{PoliticalDiversity}}.
#'
#' @importFrom utils head
#' @examples
#' votes <- sample(1:10000, 5)
#' parties <- sample(LETTERS, 5)
#' Hamilton(parties, votes, seats = 4)
#'
#' @export
#' @rdname Hamilton
`Hamilton` <-function(parties=NULL, votes=NULL, seats=NULL,...) UseMethod("Hamilton")


#' @export
#' @rdname Hamilton
`Hamilton` <-function(parties=NULL, votes=NULL, seats=NULL,...){
  # Modified :
  # v0.0 2011-10-25
  # v0.1 2012-07-10
  # v0.2 2016-01-05
  # v0.2 2016-05-15
  output <- data.frame(
    parties = parties,
    scores = votes / sum(votes) * seats,
    perc = round(votes / sum(votes),3));
  integer <- with(output, floor(scores));
  fraction <- with(output, scores - integer);
  remainder <- seats - sum(integer);
  output[,2] <- integer;
  extra <- utils::head(order(fraction, decreasing=TRUE), remainder);
  output$scores[extra] <- (output$scores[extra] + 1);
  if(sum(output$scores) != seats)
    stop("Allocation error.");
  names(output) <-c("Party", "Seats", "\u0025 Seats");
  class(output) <- c("SciencesPo", class(output))
  attr(output, "scpo.type") <- "Standard"
  return(output)
}
NULL





#' @encoding UTF-8
#' @title The D'Hondt Method of Allocating Seats Proportionally
#'
#' @description The function calculates seats allotment in legislative house,
#' given the total number of seats and the votes for each party based on the
#' Victor D'Hondt's method (1878). The D'Hondt's method is mathematically
#' equivalent to the method proposed by Thomas Jefferson few years before (1792).
#'
#' @param parties a vector containig parties labels or candidates accordingly to the \code{votes} vector order.
#' @param votes a vector containing the total number of formal votes received by the parties/candidates.
#' @param seats an integer for the number of seats to be filled (the district magnitude).
#' @param \dots Additional arguements (currently ignored)
#'
#' @return A \code{data.frame} of length \code{parties} containing apportioned integers (seats) summing to \code{seats}.
#'
#' @keywords Electoral
#' @references
#'  Lijphart, Arend (1994). \emph{Electoral Systems and Party Systems: A Study of Twenty-Seven Democracies, 1945-1990}. Oxford University Press.
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}.
#' @seealso \code{\link{HighestAverages}}, \code{\link{LargestRemainders}},  \code{\link{Hamilton}}, \code{\link{PoliticalDiversity}}.
#'
#' @note Adapted from Carlos Bellosta's replies in the R-list.
#'
#' @examples
#' votes <- sample(1:10000, 5)
#' parties <- sample(LETTERS, 5)
#'
#' dHondt(parties, votes, seats = 4)
#'
#'
#' # Example: 2014 Brazilian election for the lower house in
#' # the state of Ceara. Coalitions were leading by the
#' # following parties:
#'
#' results <- c(DEM=490205, PMDB=1151547, PRB=2449440,
#' PSB=48274, PSTU=54403, PTC=173151)
#'
#' dHondt(parties=names(results), votes=results, seats=19)
#'
#'
#' @importFrom utils head
#' @rdname dHondt
#' @export
`dHondt` <- function(parties=NULL, votes=NULL, seats=NULL, ...) UseMethod("dHondt")

#' @rdname dHondt
#' @export
`dHondt` <-function(parties=NULL, votes=NULL, seats=NULL, ...){
  # Modified :
  # v0.0 2011-10-25
  # v0.1 2012-07-10
  # v0.2 2016-01-05
  # creates a party score object
  .temp <- data.frame(
    parties = rep(parties, each = seats ),
    scores = as.vector(sapply( votes, function(x) x /
                                 1:seats ))
  );
  output <- with(.temp, (parties[order(-scores)][1:seats]))
  output <- freq(output, digits = 3);
  names(output) <-c("Party", "Seats", "\u0025 Seats");
  # output <- output[ order(output[,2], decreasing = TRUE),]
  class(output) <- c("SciencesPo", class(output))
  attr(output, "scpo.type") <- "Standard"
  return(output)
}
NULL



