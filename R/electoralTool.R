#' @encoding UTF-8
#' @title Political Diversity Indices
#'
#' \code{politicalDiversity} is used to analyze political diversity in a unity or across them. It provides methods to estimate the Effective Number of Parties as well as other diversity measures.
#'
#' @param x A data.frame, a mtrix-like, or a vector containing values for the number of votes or seats each party received.
#' @param index The type of index desired, one of "shannon", "simpson", "invsimpson", "golosov".
#' @param margin The margin for which the index is computed.
#' @param base The logarithm base used in the "shanron" method.
#'
#' identifies the variable containing party labels. It should be used only when response variable is a frequency variable containing number of votes at the aggregate level.
#' district identifies districts. It is required when there are more than one district.
#' seats  this option can be used to tell the program the number of seats by party. If used, the program will compute proportionality and parliamentary fragmentation.
#'polar identifies variables to compute polarization among groups, such as polarization by ideology. Up to five variables are allowed. If you use more than one variable, polarization will be computed using averaged polarization over the whole set of variables. Absolute and Euclidean measures of polarization are reported.
#' time tells the program that the data contains more than one election. This option identifies the date of the election or the order in which elections take place. Using this option means that the program will compute electoral and parliamentary volatitlity between elections.
#' blocks tells the program that parties are grouped into blocks to compute inter and intra blocks volatility. You can only use it if you set time(varname) previously. If you are using time(varname), but do not set blocks(varname), all the parties are suppossed to belong to the same block. Then, inter-blocks volatility will be equal to 0, and intra-blocks volatility will be equal to total volatility.
#' verbose if \code{verbose=TRUE}, tells the program to print the output.
#'
#'The Effective Number of Parties \bold{ENP} is a measure of fragmentation. The intutiton is to count parties while weighting them by their relative strength in the legislature.

#' @details Very often, political analysts say things like \sQuote{two-party system} and \sQuote{multi-party system} to refer to a particular kind of political party systems. However, these terms alone does not tell exactly how fragmented--or concentrated a party system actually is. For instance, after the 2010 general election in Brazil, 22 parties obtained representation in the country's Lower Chamber. Nonetheless, nine parties returned only 28 MPs together. Thus, an algorithm to (weigh) or to calculate the \bold{Effective Number of Parties} in such circumstances helps to go beyond the simple number of parties in a legislative branch.
#'
#' A widely accepted algorithm was proposed by M. Laakso and R. Taagepera: \deqn{N = \frac{1}{\sum p_i^2}}{N = 1/ \sum p_i^2}, where \bold{N} denotes the effective number of parties and \bold{p_i} denotes the \eqn{it^h} party's fraction of the seats.
#'
#' The same process can be used to compute the vote share for each party. This formula is the reciprocal of a well-known concentration index (\bold{the Herfindahl-Hirschman index}) used in economics to study the degree to which ownership of firms in an industry is concentrated. Laakso and Taagepera correctly saw that the effective number of parties is simply an instance of the inverse measurement problem to that one. This index makes rough but fairly reliable international comparisons of party systems possible.
#'
#' Another measure is the \bold{Least squares index (lsq)}, which measures the disproportionality produced by the election. Specifically, by the disparity between the distribution of votes and seats allocation.
#'
#' Recently, Grigorii Golosov proposed a new method for computing the effective number of parties  in which both larger and smaller parties are not attributed unrealistic scores as those resulted by using the Laakso/Taagepera index.I will call this as (\bold{Golosov}) and is given by the following formula: \deqn{N = \sum_{i=1}^{n}\frac{p_{i}}{p_{i}+p_{i}^{2}-p_{i}^{2}}}
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}.
#'
#'  @references Gallagher, Michael and Paul Mitchell (2005) \emph{The Politics of Electoral Systems.} Oxford University Press.
#'  @references Golosov, Grigorii (2010) The Effective Number of Parties: A New Approach, \emph{Party Politics,} \bold{16:} 171-192.
#'  @references Laakso, Markku and Rein Taagepera (1979) Effective Number of Parties: A Measure with Application to West Europe, \emph{Comparative Political Studies,} \bold{12:} 3-27.
#'  @references Nicolau, Jairo (2008) \emph{Sistemas Eleitorais.} Rio de Janeiro, FGV.
#'  @references Taagepera, Rein and Matthew S. Shugart (1989) \emph{Seats and Votes: The Effects and Determinants of Electoral Systems.} New Haven: Yale University Press.
#'
#' @examples
#' # Here are some examples, help yourself:
#' A <- c(.75,.25)
#' B <- c(.35,.35,.30)
#' C <- c(.75,.10,rep(0.01,15))
#'
#' # Non-trivial example:
#' # The 1980 presidential election in the US (vote share):
#' party_1980 <- c("Democratic", "Republican", "Independent", "Libertarian", "Citizens", "Others")
#' US1980 <- c(0.410, 0.507, 0.066, 0.011, 0.003, 0.003)
#'
#' politicalDiversity(US1980, index= "herfindahl")
#' # or
#' politicalDiversity(US1980, index= "simpson")
#'
#' party_2004 <- c("Democratic", "Republican", "Independent", "Libertarian",
#' "Constitution", "Green", "Others")
#' US2004 <- c(0.481, 0.509, 0.0038, 0.0032, 0.0012, 0.00096, 0.00084)
#'
#' politicalDiversity(US2004, index= "herfindahl")
#' # The 1999 Finland election
#'votes_1999 <- c(612963, 600592, 563835, 291675, #' 194846, 137330, 111835, 28084, 26440, 28549, 20442, 10378, 10104, 5451, 5194, 4481, 3903, 3455, 21734)
#'
#' seats_1999 <- c(51, 48, 46, 20, 11, 11, 10, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
#'
#'
#' # Helsinki's 1999
#' votes_1999 <- c(68885,18343, 86448, 21982, 51587,
#' 27227, 8482, 7250, 365, 2734, 1925, 475,
#' 1693, 693, 308, 980, 560, 590, 185)
#'
#' #Sainte-Lague allocation:
#' seats_1999sl <- c(5, 1, 6, 1, 4, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
#' # D'Hondt allocation:
#' seats_1999dh <- c(5, 1, 7, 1, 4, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
#'
#'
#' # 2010 Brazilian legislative election
#'
#' party_2010 = c("PT","PMDB","PSDB", "DEM","PR","PP","PSB","PDT","PTB", "PSC","PV",
#' "PC do B","PPS","PRB", "PMN", "PT do B", "PSOL","PHS","PRTB","PRP","PSL","PTC")
#' votes_2010 = c(13813587, 11692384, 9421347, 6932420, 7050274, 5987670, 6553345,
#' 4478736, 3808646, 2981714,2886633, 2545279, 2376475, 1659973, 1026220,
#' 605768, 968475, 719611, 283047, 232530, 457490, 563145)
#'
#' # 2010 Election outcome as proportion of seats
#' seats_2010 = c(88, 79, 53, 43, 41, 41, 34, 28, 21,
#' 17, 15, 15, 12, 8, 4, 3, 3, 2, 2, 2, 1, 1)/513
#'
#' # 2014 Election outcome as proportion of seats
#' seats_2014 = c(70, 66, 55, 37, 38, 34, 34, 26, 22, 20, 19, 15, 12,
#' 11, 10, 9, 8, 5, 4, 3, 3, 3, 2, 2, 2, 1, 1, 1)/513
#'
#' politicalDiversity(seats_2014, index= "laakso/taagepera")
#' # or:
#' politicalDiversity(seats_2014, index= "invsimpson")
#'
#' @keywords Basics
#' @keywords Electoral
#' @export
politicalDiversity <-
function (x, index = "herfindahl", margin = 1, base = exp(1))
{
  x <- drop(as.matrix(x))
  methods <- c("shannon", "simpson", "invsimpson", "golosov", "laakso/taagepera", "herfindahl", "lsq", "ENC", "ENP")
  index <- match.arg(index, methods)
  if (length(dim(x)) > 1) {
    total <- apply(x, margin, sum)
    x <- sweep(x, margin, total, "/")
  }
  else {
    x <- x/sum(x)
  }
  if (index == "shannon")
    x <- -x * log(x, base)
  else x <- x * x
  if (length(dim(x)) > 1)
    H <- apply(x, margin, sum, na.rm = TRUE)
  else H <- sum(x, na.rm = TRUE)
  if (index == "simpson"||index == "herfindahl" )
    H <- 1 - H
  else if (index == "invsimpson"|| index == "laakso/taagepera" || index == "ENC" || index == "ENP")
    H <- 1/H
  return(round(H, 3))
}
NULL


#' # hareQuota <- function(votes, seats, try.quota, droop.quota){}
#'
#'# electoralTool <- function(party,
#'#  district, seats, polar, time, blocks,
#'#   verbose=TRUE){
#'#   if(verbose)}
