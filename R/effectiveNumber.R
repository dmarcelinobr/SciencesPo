#' @encoding UTF-8
#' @title Computes the Effective Number of Parties
#'
#' @description The Effective Number of Parties \bold{ENP} is a measure of fragmentation. The intutiton is to count parties while weighting them by their relative strength in the legislature.

#' @param seats a numeric value or a vector (may be as proportion
#' @param votes a numeric value or a vector as the number of votes received by each party
#' @param total a numeric value for the total either of seats or votes
#' @param method the method using for computing the ENP, the default is Laakso/Taagepera, see details below. 
#'
#' @return The Effective Number of Parties
#'
#' @details Very often, political analysts say things like \sQuote{two-party system} and \sQuote{multi-party system} to refer to a particular kind of political party systems. However, these terms alone does not tell exactly how fragmented--or concentrated a party system actually is. For instance, after the 2010 general election in Brazil, 22 parties obtained representation in the country's Lower Chamber. Nonetheless, nine parties returned only 28 MPs together. Thus, an algorithm to (weigh) or to calculate the \bold{Effective Number of Parties} in such circumstances helps to go beyond the simple number of parties in a legislative branch. 
#'
#' A widely accepted algorithm was proposed by M. Laakso and R. Taagepera: \deqn{N = \frac{1}{\sum p_i^2}}{N = 1/ \sum p_i^2}, where \bold{N} denotes the effective number of parties and \bold{p_i} denotes the \deqn{it^h} party's fraction of the seats. 
#'
#' The same process can be used to compute the vote share for each party. This formula is the reciprocal of a well-known concentration index (\bold{the Herfindahl-Hirschman index}) used in economics to study the degree to which ownership of firms in an industry is concentrated. Laakso and Taagepera correctly saw that the effective number of parties is simply an instance of the inverse measurement problem to that one. This index makes rough but fairly reliable international comparisons of party systems possible.
#'
#' Another measure is the \bold{Least squares index (LSq)}, which typically measures the disproportionality produced by the election. Specifically, by the disparity between the distribution of votes and seats allocation. 
#'  
#' Recently, Grigorii Golosov proposed a new method for computing the effective number of parties  in which both larger and smaller parties are not attributed unrealistic scores as those resulted by using the Laakso—Taagepera index.I will call this as (\bold{Golosov}) and is given by the following formula: \deqn{N = \sum_{i=1}^{n}\frac{p_{i}}{p_{i}+p_{i}^{2}-p_{i}^{2}}}

#' @note So far, I have implemented the following methods: LSq, Laakso and Taagepera, and Golosov.

#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' 
#'  @references Gallagher, Michael and Paul Mitchell (2005) \emph{The Politics of Electoral Systems.} Oxford University Press.
#'  @references Golosov, Grigorii (2010) The Effective Number of Parties: A New Approach, \emph{Party Politics,} \bold{16:} 171--192. \bold{DOI: 10.1177/1354068809339538}.
#'  @references Laakso, Markku and Rein Taagepera (1979) Effective Number of Parties: A Measure with Application to West Europe, \emph{Comparative Political Studies,} \bold{12:} 3-–27. \bold{DOI: 10.1177/001041407901200101}.
#'  @references Nicolau, Jairo (2008) \emph{Sistemas Eleitorais.} Rio de Janeiro, FGV.
#'  @references Taagepera, Rein and Matthew S. Shugart (1989) \emph{Seats and Votes: The Effects and Determinants of Electoral Systems.} New Haven: Yale University Press.
#'
#' @examples
#' # Here are some examples help yourself:
#' A <- c(.75,.25)
#' B <- c(.35,.35,.30)
#' C <- c(.75,.10,rep(0.01,15))
#'
#'effectiveNumber(seats=A, votes=NULL, total=1)
#' effectiveNumber(seats=B, votes=NULL, total=1, method="Golosov")
#'
#' # Non-trivial example: 
#' # 2010 Election outcome
#' party = c("PT","PMDB","PSDB", "DEM","PR","PP","PSB","PDT","PTB", "PSC","PV",
#' "PC do B","PPS","PRB", "PMN", "PT do B", "PSOL","PHS","PRTB","PRP","PSL","PTC")
#' votes = c(13813587, 11692384, 9421347, 6932420, 7050274, 5987670, 6553345, 
#' 4478736, 3808646, 2981714,2886633, 2545279, 2376475, 1659973, 1026220, 
#' 605768, 968475, 719611, 283047, 232530, 457490, 563145)
#'
#' # 2010 Election outcome passed as proportion of seats
#' seats_2010 = c(88,79,53,43,41,41,34,28,21,17,15,15,12,8,4,3,3,2,2,2,1,1)/513
#'
#' effectiveNumber(seats=seats_2010, votes=NULL, total=NULL, method="Golosov")
#'
#' # 2014 Election outcome passed as proportion of seats
#' seats_2014 = c(70,66,55,37,38,34,34,26,22,20,19,15,12,11,10,9,8,5,4,3,3,3,2,2,2,1,1,1)/513
#'
#' effectiveNumber(seats=seats_2014, votes=NULL, total=NULL, method="Golosov")
#' 
#' @keywords The Basics
#' @keywords Political Behavior 
#'
#' @export
#'
#'
effectiveNumber <-
function(seats=NULL, votes=NULL, total=NA, method="Laakso/Taagepera"){
  if(method=="Golosov"){
      if(!is.null(seats)){
        round(sum((seats)/((seats)+((seats[1])^2)-((seats)^2))),2) -> Golosov
      } else{
        # round(sum((votes)/((votes)+((votes[1])^2)-((votes)^2))),2) -> Golosov 
      }
      return(Golosov)
    } 
  if(method=="LSq"){
    if(!is.null(seats)){
      round(sum((seats)/((seats)+((seats[1])^2)-((seats)^2))),2) -> LSq
    } else{
      
      # round(sum((votes)/((votes)+((votes[1])^2)-((votes)^2))),2) ->LSq 
    }
  return(LSq) 
  
  }else {
      if(!is.null(seats)){
        round(1/sum(table(seats)*(seats/sum(seats))^2),2) -> invHHIs
      }else{
        # round(sum(table(votes))^2/sum(table(votes)^2),2) -> invHHIs
        }
    return(invHHIs)
    }
}