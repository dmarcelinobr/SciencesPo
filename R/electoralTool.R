#' @encoding UTF-8
#' @title Political Diversity Indices
#'
#' \code{politicalDiversity} is used to analyze political diversity in a unity or across them. It provides methods to estimate the Effective Number of Parties as well as other diversity measures.
#'
#' @param x A data.frame, a matrix-like, or a vector containing values for the number of votes or seats each party received.
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
#' A <- c(.75,.25);
#' B <- c(.35,.35,.30);
#' C <- c(.75,.10,rep(0.01,15))
#' 
#' politicalDiversity(A, index= "laakso/taagepera")
#' 
#' # Non-trivial example:
#' # The 1980 presidential election in the US (vote share):
#' party_1980 <- c("Democratic", "Republican", "Independent", "Libertarian", "Citizens", "Others")
#' US1980 <- c(0.410, 0.507, 0.066, 0.011, 0.003, 0.003)
#'
#' politicalDiversity(US1980, index= "laakso/taagepera")
#'
#' politicalDiversity(US1980, index= "herfindahl")
#'
#'
#' party_2004 <- c("Democratic", "Republican", "Independent", "Libertarian",
#' "Constitution", "Green", "Others")
#' US2004 <- c(0.481, 0.509, 0.0038, 0.0032, 0.0012, 0.00096, 0.00084)
#'
#' politicalDiversity(US2004, index= "herfindahl")
#'
#' # Using Grigorii Golosov approach:
#' politicalDiversity(US2004, index= "golosov")
#'
#' # The 1999 Finland election
#' votes_1999 <- c(612963, 600592, 563835,
#' 291675, 194846, 137330, 111835, 28084, 26440, 28549, 20442, 10378,
#' 10104, 5451, 5194, 4481, 3903, 3455, 21734)
#'
#' seats_1999 <- c(51, 48, 46, 20, 11, 11, 10, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
#'
#' # Helsinki's 1999
#' votes_1999 <- c(68885,18343, 86448,
#' 21982, 51587, 27227, 8482, 7250, 365,
#' 2734, 1925, 475, 1693, 693, 308, 980,
#' 560, 590, 185)
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
#'
#' # or:
#'
#' politicalDiversity(seats_2014, index= "invsimpson")
#'
#' politicalDiversity(seats_2014, index= "golosov")
#'
#' @keywords Basics
#' @keywords Electoral
#' @export
politicalDiversity <-
function (x, index = "herfindahl", margin = 1, base = exp(1))
{
  x <- drop(as.matrix(x))
  methods <- c("enc", "enp", "herfindahl", "laakso/taagepera", "gini", "golosov", "shannon", "simpson", "invsimpson", "lsq")
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
  else if (index=="golosov")
    x <- sum((x)/((x)+((x[1])^2)-((x)^2)))
  else x <- x * x
  if (length(dim(x)) > 1)
    idx <- apply(x, margin, sum, na.rm = TRUE)
  else idx <- sum(x, na.rm = TRUE)
  if (index == "simpson"||index == "herfindahl" || index ==  "gini" )
    idx <- 1 - idx
  else if (index == "invsimpson"|| index == "laakso/taagepera" || index == "ENC" || index == "ENP")
    idx <- 1/idx
  return(round(idx, 3))
}
NULL


#' @title Gini Simpson Index
#'
#' @param x A data.frame, a matrix-like, or a vector.
#' @param na.rm A logical value to deal with NAs.
#'
#' @export
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}.
#'
#' @importFrom stats na.omit
#' @seealso \code{\link{politicalDiversity}}.
#' @examples
#' x <- as.table(c(69,17,7,62))
#' rownames(x) <- c("AB","C","D","0")
#' gini.simpson(x)
#'
gini.simpson <-
  function(x, na.rm = FALSE) {
    # reference:   Sachs, Angewandte Statistik, S. 57
    if(na.rm) x <- na.omit(x)
    x <- as.table(x)
    ptab <- prop.table(x)
    return(sum(ptab*(1-ptab)))
  }
NULL

# weighted gini
gini <-function (x, weights = rep(1, length = length(x)))
{
  ox <- order(x)
  x <- x[ox]
  weights <- weights[ox]/sum(weights)
  p <- cumsum(weights)
  nu <- cumsum(weights * x)
  n <- length(nu)
  nu <- nu/nu[n]
  sum(nu[-1] * p[-n]) - sum(nu[-n] * p[-1])
}
NULL

#' @title Gallagher index
#'
#' @description Calculates the Gallagher index of LSq index.
#'
#' @param v A vector containing the votes for each political party.
#' @param s A vector containing the election outcome as seats.
#'
#' @details The representation score is calculated as: sqrt(sum((Z-R)^2)/2).
#' @return The Gallagher's Representation Score.
#' @author Daniel Marcelino \email{dmarcelino@@live.com}
#'
#' @references
#'  Gallagher, M. (1991) Proportionality, disproportionality and electoral systems. Electoral Studies 10(1):33-51.
#'  @examples
#' # 2005 UK General Election
#' pvotes = c(Lab=35.20, Cons=32.40, Lib=22, DUP=0.90,
#' SNP=1.50, Sinn.Fein=0.60, Plaid=0.60, SDLP=0.50, UUP=0.50,
#' Ind=0.50, Respect=0.30, Health=0.10, Speaker=0.10, Others=4.80)
#' seats = c(385,198, 62, 9,6,5,3,3,1,1,1,1,1,0)/676
#'
#' # 2012 Queensland state elecion
#' pvotes= c(49.65, 26.66, 11.5, 7.53, 3.16, 1.47)
#' pseats = c(87.64, 7.87, 2.25, 0.00, 2.25, 0.00)
#'
#' gallagher(pvotes, pseats)
#'
#' @export
gallagher <- function(v,s) {
  idx=sqrt(sum((v-s)^2)/2)
  return(round(idx, 3))
}
NULL


#' @title Lijphart index of proportionality
#'
#' @description Calculates the Lijphart index of proportionality based on a vector of votes and a vector for the electoral outcome.
#'
#' @param v A vector containing the votes for each political party.
#' @param s A vector containing the election outcome as seats.
#' @return A single score given the vector of votes and the vector for seats.
#' @author Daniel Marcelino \email{dmarcelino@@live.com}
#'
#' # 2012 Quebec provincial election:
#' pvotes = c(PQ=31.95, Lib=31.20, CAQ=27.05,QS=6.03,Option=1.89, Other=1.88)
#' pseats = c(PQ=54, Lib=50, CAQ=19, QS=2, Option=0, Other=0)
#'
#' lijphart(pvotes, pseats)
#'
#' @export
lijphart <- function(v,s) {
  idx=max(s-v)
  return(round(idx, 3))
}
NULL




#' @title Grofman index
#'
#' @description Calculates the Grofman index of proportionality based on a vector of votes and a vector for the electoral outcome.
#'
#' @param v A vector containing the votes for each political party.
#' @param s A vector containing the election outcome as seats.
#'
#' @return A single score given the vector of votes and the vector for seats.
#'
#'  @references
#' Taagepera, R., and B. Grofman. Mapping the indices of seats-votes disproportionality and inter-election volatility. Party Politics 9, no. 6 (2003): 659-77.
#'
#' @author Daniel Marcelino \email{dmarcelino@@live.com}
#' @examples
#' # 2012 Quebec provincial election:
#' pvotes = c(PQ=31.95, Lib=31.20, CAQ=27.05,QS=6.03,Option=1.89, Other=1.88)
#' pseats = c(PQ=54, Lib=50, CAQ=19, QS=2, Option=0, Other=0)
#'
#' grofman(pvotes, pseats)
#'
#' @export
grofman <- function(v,s) {
  N <- politicalDiversity(s, index = "laakso/taagepera")
  idx=(1/N) * sum(abs(v-s))/2
  return(round(idx, 3))
}
NULL




#' @title Farina index
#'
#' @description Calculates the Farina index also referred to as the cosine proportionality score based on a vector of votes and a vector for the electoral outcome.
#' @param v A vector containing the votes for each political party.
#' @param s A vector containing the election outcome as seats.
#' @return A single score given the vector of votes and the vector for seats.
#'
#' @author Daniel Marcelino \email{dmarcelino@@live.com}
#' @references
#' Koppel, M., and A. Diskin. (2009) Measuring disproportionality, volatility and malapportionment: axiomatization and solutions. Social Choice and Welfare 33, no. 2: 281-286.
#'
#' @examples
#'
#' # 2012 Queensland state elecion
#' pvotes= c(49.65, 26.66, 11.5, 7.53, 3.16, 1.47)
#' pseats = c(87.64, 7.87, 2.25, 0.00, 2.25, 0.00)
#'
#' farina(pvotes, pseats)
#'
#' @export
farina <- function(v,s) {
  idx= acos(sum(v*s)/(sum(v^2)*sum(s^2))^.5)
  return(round(idx, 3))
}
NULL



#' @title Cox-Shugart measure of proportionality
#'
#' @description Calculate the Cox and Shugart measure of
#'  proportionalitybased on a vector of votes and a vector for
#'  the electoral outcome. This measure is also referred to as the regression index.
#' @param v A vector containing the votes for each political party.
#' @param s A vector containing the election outcome as seats.
#'
#' @return A single score given the vector of votes and the vector for seats.
#' @author Daniel Marcelino \email{dmarcelino@@live.com}
#'
#' @examples
#' # 2012 Queensland state elecion:
#' pvotes= c(49.65, 26.66, 11.5, 7.53, 3.16, 1.47)
#' pseats = c(87.64, 7.87, 2.25, 0.00, 2.25, 0.00)
#'
#' cox.shugart(pvotes, pseats)
#'
#' # 2012 Quebec provincial election:
#' pvotes = c(PQ=31.95, Lib=31.20, CAQ=27.05, QS=6.03, Option=1.89, Other=1.88)
#' pseats = c(PQ=54, Lib=50, CAQ=19, QS=2, Option=0, Other=0)
#'
#' cox.shugart(pvotes, pseats)
#'
#' @export
cox.shugart <- function(v,s) {
  S <- mean(s)
  V <- mean(v)
  idx <- sum((s-S) * (v-V))/sum((v-V)^2)
  return(round(idx, 3))
}
NULL



inv.cox.shugart <- function(v,s) {
  V <- mean(v)
  S <- mean(s)
  idx <- sum((v-V) * (s-S))/sum((s-S)^2)
    return(round(idx, 3))
}
NULL


#' # hareQuota <- function(votes, seats, try.quota, droop.quota){}
#'
#'# electoralTool <- function(party,
#'#  district, seats, polar, time, blocks,
#'#   verbose=TRUE){
#'#   if(verbose)}

#'# votes86 = c(54109, 365376, 14325, 241402, 478405, 11788, 3055097, 2668055, 7973080, 50590, 13840, 1236493, 81424, 84156, 19520016, 41280, 6601, 28194, 27399, 35768, 1679, 27202, 50669, 387733, 166179, 2537691, 1722801 ,5844, 30907)


#'# 2010: ALL
#'# votes10=c(6932420,2580925, 22936, 1425, 4579006, 720154, 11867988, 1052218, 6862846, 2376475, 7097712, 1659973, 231282, 291341, 6582622, 2965055, 9500548, 177773, 464180, 969954, 54252, 14251798, 605768, 3912093, 565409, 155434, 2885915)

#'# only Elected:
#'# votes10=c(4550184,1823077,2596556, 96060, 8492579, 446162, 5197306, 1356469, 5889206, 871461, 47488, 50488, 4737508, 1859443, 6362262, 40093, 442756, 10472717, 178734, 2349527, 104015, 1005770)


#'#ro <- c(50740, 80954)

#'# results <- c(DEM=490205, PMDB=1151547, PRB=2449440, PSB=48274, PSTU=54403, PTC=173151)

#'# votes14=c(DEM=3868200, PCdoB=1799619, PCB=37253, PCO=8267, PDT=3141818, PEN=634682, PHS=903968, PMDB=10053108, PMN=432807, PP=6158835, PPL=103606, PPS=1875826, PR=5448721, PRB=4297373, PROS=1879940, PRP=655107, PRTB=430995, PSB=5574401, PSC=2420581, PSD=5637961, PSDB=9145950, PSDC=491280, PSL=772628, PSOL=1486393, PSTU=151353, PT=11803985, PTdoB=807509, PTB=3703639, PTC=312548, PTN=682854, PV=1808991, SD=2621639)
