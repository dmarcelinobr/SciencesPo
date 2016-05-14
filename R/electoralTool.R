#' @encoding UTF-8
#' @title Political Diversity Indices
#'
#' @description Analyzes political diversity in an electoral unity or across unities. It provides methods for estimating the effective number of parties and other fragmentation/concetration measures. The intuition of these coefficients is to counting parties while weighting them by their relative political--or electoral strength.
#'
#' @param x A data.frame, a matrix-like, or a vector containing values for the number of votes or seats each party received.
#' @param index The type of index desired, one of "laakso/taagepera",  "golosov", "herfindahl", "gini", "shannon", "simpson", "invsimpson".
#' @param margin The margin for which the index is computed.
#' @param base The logarithm base used in some indices, such as the "shannon" index.
#'
#' @details Very often, political analysts say things like \sQuote{two-party system} and \sQuote{multi-party system} to refer to a particular kind of political party system. However, these terms alone does not tell exactly how fragmented--or concentrated a party system actually is. For instance, after the 2010 general election, 22 parties obtained representation in the Lower Chamber in Brazil. Nonetheless, among these 22 parties, nine parties together returned only 28 MPs. Thus, an index to assess the weigh or the \bold{Effective Number of Parties} is important and helps to go beyond the simple count of parties in a legislative branch.
#'
#' A widely accepted algorithm was proposed by M. Laakso and R. Taagepera: \deqn{N = \frac{1}{\sum p_i^2}}{N = 1/ \sum p_i^2}, where \bold{N} denotes the effective number of parties and \bold{p_i} denotes the \eqn{it^h} party's fraction of the seats.
#'
#' In fact, this formula may be used to compute the vote share for each party. This formula is the reciprocal of a well-known concentration index (\bold{the Herfindahl-Hirschman index}) used in economics to study the degree to which ownership of firms in an industry is concentrated. Laakso and Taagepera correctly saw that the effective number of parties is simply an instance of the inverse measurement problem to that one. This index makes rough but fairly reliable international comparisons of party systems possible.
#' \bold{The Inverse Simpson index},
#' \deqn{ 1/ \lambda = {1 \over\sum_{i=1}^R p_i^2} = {}^2D}
#' Where \eqn{\lambda} equals the probability that two types taken at random from the dataset (with replacement) represent the same type. This simply equals true fragmentation of order 2, i.e. the effective number of parties that is obtained when the weighted arithmetic mean is used to quantify average proportional diversity of political parties in the election of interest.
#'
#' Another measure is the \bold{Least squares index (lsq)}, which measures the disproportionality produced by the election. Specifically, by the disparity between the distribution of votes and seats allocation.
#'
#' Recently, Grigorii Golosov proposed a new method for computing the effective number of parties  in which both larger and smaller parties are not attributed unrealistic scores as those resulted by using the Laakso/Taagepera index.I will call this as (\bold{Golosov}) and is given by the following formula: \deqn{N = \sum_{i=1}^{n}\frac{p_{i}}{p_{i}+p_{i}^{2}-p_{i}^{2}}}
#'

#' @author Daniel Marcelino, \email{dmarcelino@@live.com}.
#'
#' @seealso \code{\link{CoxShugart}}, \code{\link{Rae}},  \code{\link{Rose}}, \code{\link{Farina}}, \code{\link{Grofman}}, \code{\link{Gallagher}}, \code{\link{Lijphart}}. For more details see the Indices vignette: \code{vignette("Indices", package = "SciencesPo")}
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
#' @export PoliticalDiversity
#' @rdname PoliticalDiversity
#' @aliases PoliticalFragmentation
`PoliticalDiversity`<- function(x, index = "laakso/taagepera", margin=1, base = exp(1))
  UseMethod("PoliticalDiversity")

#' @rdname PoliticalDiversity
`PoliticalDiversity`<- function(x, index = "laakso/taagepera", margin=1, base = exp(1)){
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





#' @title Indexes of Proportionality
#'
#' @description Calculates several indexes of proportionality to show the relationship of votes to seats.
#'
#' @param v A numeric vector with the percentage share of votes obtained by each party.
#' @param s A numeric vector with the percentage share of seats obtained by each party.
#' @param index The desired type of index, see details below.
#' @param \dots Additional arguements (currently ignored)
#' @return A single score.
#'
#' @author Daniel Marcelino \email{dmarcelino@@live.com}
#' @details The following measures are available:
#' \itemize{
#' \item {"Cox-Shugart"}{Cox-Shugart Measure of Proportionality}
#' \item {"inv.Cox-Shugart"}{The Inverse of Cox-Shugart Index}
#' \item {"Farina"}{Farina Index of Proportionality}
#' \item {"Gallagher"}{Gallagher Index of Disproportionality}
#' \item {"inv.Gallagher"}{The Inverse of Gallagher Index}
#' \item {"Farina"}{Farina Index of Proportionality}
#' \item {"Grofman"}{Grofman Index of Proportionality}
#' \item {"Lijphart"}{Lijphart Index of Proportionality}
#' \item {"Loosemore-Hanby"}{Loosemore-Hanby Index of Disproportionality}
#' \item {"Rae"}{Rae Index of Disproportionality}
#' \item {"inv.Rae"}{The Inverse of the Rae Index}
#' \item {"Rose"}{Rose Index of Disproportionality}
#' \item {"inv.Rose"}{The Inverse of the Rose Index}
#' }
#'
#' @seealso \code{\link{PoliticalDiversity}}, \code{\link{LargestRemainders}}, \code{\link{HighestAverages}}. For more details, see the Indices vignette: \code{vignette("Indices", package = "SciencesPo")}

#' @references
#' Duncan, O. and Duncan, B. (1955) A methodological analysis of segregation indexes. \emph{American Sociological Review} 20:210-7.
#'
#' Gallagher, M. (1991) Proportionality, disproportionality and electoral systems. Electoral Studies 10(1):33-51.
#'
#' Loosemore, J. and Hanby, V. (1971) The theoretical limits of maximum distortion: Som analytical expressions for electoral systems. \emph{British Journal of Political Science} 1:467-77.
#'
#' Koppel, M., and A. Diskin. (2009) Measuring disproportionality, volatility and malapportionment: axiomatization and solutions. Social Choice and Welfare 33, no. 2: 281-286.
#'
#' Rae, D. (1967) \emph{The Political Consequences of Electoral Laws.} London: Yale University Press.
#'
#' Rose, Richard, Neil Munro and Tom Mackie (1998) \emph{ Elections in Central and Eastern Europe Since 1990.} Glasgow: Centre for the Study of Public Policy, University of Strathclyde.
#'
#' Taagepera, R., and B. Grofman. Mapping the indices of seats-votes disproportionality and inter-election volatility. Party Politics 9, no. 6 (2003): 659-77.
#'
#'
#' @examples
#' #' # 2012 Queensland state elecion
#' pvotes= c(49.65, 26.66, 11.5, 7.53, 3.16, 1.47)
#' pseats = c(87.64, 7.87, 2.25, 0.00, 2.25, 0.00)
#'
#' # Proportionality(pvotes, pseats) # default is Gallagher
#'
#' # Proportionality(pvotes, pseats, index="Rae")
#'
#' # Proportionality(pvotes, pseats, index="Cox-Shugart")
#'
#' # 2012 Quebec provincial election:
#' pvotes = c(PQ=31.95, Lib=31.20, CAQ=27.05, QS=6.03, Option=1.89, Other=1.88)
#' pseats = c(PQ=54, Lib=50, CAQ=19, QS=2, Option=0, Other=0)
#'
#' # Proportionality(pvotes, pseats, index="Rae")
#'
#' @export
#' @docType methods
#' @rdname Proportionality
#' @aliases Disproportionality
`Proportionality`<- function(v, s, index = "Gallagher", ...) UseMethod("Proportionality")

#' @rdname Proportionality
#' @export
`Proportionality.default` <- function(v, s, index = "Gallagher", ...){
  v <- drop(as.matrix(v))
  s <- drop(as.matrix(s))
  index <- .Match(arg = index, choices = c("Cox-Shugart","inv.Cox-Shugart","Farina","Gallagher","inv.Gallagher", "Grofman", "Lijphart", "Loosemore-Hanby", "Rose", "inv.Rose", "Rae","inv.Rae") )
  if (length(dim(v)) > 1) {
    total_v <- apply(v, margin, sum)
    v <- sweep(v, margin, total_v, "/")
    total_s <- apply(s, margin, sum)
    s <- sweep(s, margin, total_s, "/")
  }
  else {
    v <- v/sum(v);
    s <- v/sum(s);
  }
  if (index=="Gallagher")
    idx <- sqrt(sum((v-s)^2)/2)
  else if (index=="inv.Gallagher"){
  V <- mean(v)
  S <- mean(s)
  idx <- sum((v-V) * (s-S))/sum((s-S)^2)
  idx <- (1 - idx)
  }
  else if (index=="Cox-Shugart"){
  S <- mean(s)
  V <- mean(v)
  idx <- sum((s-S) * (v-V))/sum((v-V)^2)
  }
  else if (index=="inv.Cox-Shugart"){
    V <- mean(v)
    S <- mean(s)
    idx <- sum((v-V) * (s-S))/sum((s-S)^2)
  }
  else if (index == "Grofman"){
  N <- (1/sum((s/sum(s))^2) )
  idx=(1/N) * sum(abs(v-s))/2
  }
  else if (index == "Farina")
    idx= acos(sum(v*s)/(sum(v^2)*sum(s^2))^.5)
  else if (index == "Lijphart")
    idx=max(s-v)
  else if (index == "Rose")
    idx <- 1-(sum(abs(v-s))/2)
  else if (index=="Rae")
    idx <- (sum(abs(v - s))/length(v))
  else if (index=="inv.Rae"){
    idx <- (sum(abs(v - s))/length(v))
    idx <- (1 - idx)
  }
  else if (index=="Loosemore-Hanby")
    idx <- (sum(abs(v-s))/2)
  else if (index=="inv.Rae")
    idx <- (sum(abs(v - s))/length(v))
  else if (index=="Loosemore-Hanby")
    idx <- (sum(abs(v-s))/2)
  else
    warning(paste(index), " is not a valid index name. See `details` in the function documentation.")
  print(idx, digits = max(3, getOption("digits") - 3))
}### end -- proportionality function
NULL






#' @title Loosemore-Hanby Index of Disproportionality
#'
#' @description Calculates the Loosemore-Hanby index of disproportionality.
#'
#' @param v A numeric vector with the percentage share of votes obtained by each party.
#' @param s A numeric vector with the percentage share of seats obtained by each party.
#' @param \dots Additional arguements (currently ignored)
#'
#' @details The score is calculated as the sum of the absolute differences \code{|v-s|} divided by two. This index is also known as the Pedersen index, with common usage in studies of volatility.
#' @return A single score.
#'
#' @author Daniel Marcelino \email{dmarcelino@@live.com}
#'
#' @seealso  \code{\link{Rose}}, \code{\link{Rae}}, \code{\link{CoxShugart}}, \code{\link{PoliticalDiversity}}, \code{\link{Grofman}}, \code{\link{Farina}},  \code{\link{Lijphart}}. For more details see the Indices vignette: \code{vignette("Indices", package = "SciencesPo")}
#'
#' @references
#' Duncan, O. and Duncan, B. (1955) A methodological analysis of segregation indexes. \emph{American Sociological Review} 20:210-7.
#'
#' Loosemore, J. and wHanby, V. (1971) The theoretical limits of maximum distortion: Som analytical expressions for electoral systems. \emph{British Journal of Political Science} 1:467-77.
#'
#'  @examples
#' # 2012 Queensland state elecion
#' pvotes= c(49.65, 26.66, 11.5, 7.53, 3.16, 1.47)
#' pseats = c(87.64, 7.87, 2.25, 0.00, 2.25, 0.00)
#'
#' LoosemoreHanby(pvotes, pseats)
#'
#' @export
#' @rdname LoosemoreHanby
`LoosemoreHanby`<- function(v, s, ...) UseMethod("LoosemoreHanby")

#' @export
#' @rdname LoosemoreHanby
`LoosemoreHanby` <-function(v, s, ...){
  idx=(sum(abs(v-s))/2)
  print(idx, digits = max(3, getOption("digits") - 3))
}### end -- loosemore.hanby function
NULL




#' @title Rose Index of Proportionality
#'
#' @description Calculates the Rose index of proportionality to show the relationship of votes to seats. The Rose index is a standardized version of the Loosemore-Hanby index.
#'
#' @param v A numeric vector with the percentage share of votes obtained by each party.
#' @param s A numeric vector with the percentage share of seats obtained by each party.
#' @param \dots Additional arguements (currently ignored)
#' @details The score is calculated  as the difference between a party's
#'  percentage share of the vote and its percentage share of the total seats
#'  in Parliament, summed, divided by two and subtracted from 100. Theoretically it can range from 0 to 100.
#' @return A single score.
#'
#' @author Daniel Marcelino \email{dmarcelino@@live.com}
#'
#' @seealso  \code{\link{LoosemoreHanby}}, \code{\link{Rae}}, \code{\link{CoxShugart}}, \code{\link{CoxShugart}}, \code{\link{PoliticalDiversity}}, \code{\link{Grofman}}, \code{\link{Farina}},  \code{\link{Lijphart}}. For more details see the Indices vignette: \code{vignette("Indices", package = "SciencesPo")}
#'
#' @references
#' Rose, Richard, Neil Munro and Tom Mackie (1998) \emph{ Elections in Central and Eastern Europe Since 1990.} Glasgow: Centre for the Study of Public Policy, University of Strathclyde.
#'
#'  @examples
#' # 2012 Queensland state elecion
#' pvotes= c(49.65, 26.66, 11.5, 7.53, 3.16, 1.47)
#' pseats = c(87.64, 7.87, 2.25, 0.00, 2.25, 0.00)
#'
#' Rose(pvotes, pseats)
#'
#' pvotes= c(.4965, .2666, .115, .0753, .0316, .0147)
#' pseats = c(.8764, .0787, .0225, 0.00, .0225, 0.00)
#'
#' @export
#' @rdname Rose
`Rose`<- function(v, s, ...) UseMethod("Rose")

#' @export
#' @rdname Rose
`Rose` <-function(v, s, ...){

  if (sum(v)!=1 && sum(s)!=1 || sum(v)!=100 && sum(s)!=100){
    cat("Values in", v, " or ", s, "do not sum either to 1 or 100", "\n", sep = "")
  }
  if (isTRUE(round(sum(v),0)<=1) && isTRUE(round(sum(s),0)<=1)){
    v = v*100; s = s*100;
    idx <- 100-(sum(abs(v-s))/2)
    }
  else{
    idx <- 100-(sum(abs(v-s))/2)
  }
  print(idx, digits = max(3, getOption("digits") - 3))
}### end -- rose function
NULL








#' @title Rae Index of Disproportionality
#'
#' @description Calculates the Rae index of disproportionality.
#'
#' @param v A numeric vector with the percentage share of votes obtained by each party.
#' @param s A numeric vector with the percentage share of seats obtained by each party.
#' @param \dots Additional arguements (currently ignored)
#'
#' @details The score is calculated as the sum of the absolute differences \code{|v-s|} divided by the number of parties.
#' @return A single score (The Rae's index or Rae's index of disproportionality).
#'
#' @author Daniel Marcelino \email{dmarcelino@@live.com}
#'
#' @seealso  \code{\link{CoxShugart}}, \code{\link{PoliticalDiversity}}, \code{\link{Grofman}}, \code{\link{Farina}}, \code{\link{Lijphart}}. For more details, see the Indices vignette: \code{vignette("Indices", package = "SciencesPo")}
#'
#' @references
#' Rae, D. (1967) \emph{The Political Consequences of Electoral Laws.} London: Yale University Press.
#'
#'  @examples
#' # 2012 Queensland state elecion
#' pvotes= c(49.65, 26.66, 11.5, 7.53, 3.16, 1.47)
#' pseats = c(87.64, 7.87, 2.25, 0.00, 2.25, 0.00)
#'
#' Rae(pvotes, pseats)
#'
#' @export
#' @rdname Rae
`Rae`<- function(v, s, ...) UseMethod("Rae")

#' @export
#' @rdname Rae
`Rae` <-function(v, s, ...){
  idx=(sum(abs(v-s))/length(v))
  print(idx, digits = max(3, getOption("digits") - 3))
}### end -- rae function
NULL





#' @title The Inverse Rae index
#'
#' @description Calculates the inverse of the Rae index.
#'
#' @param v A numeric vector with the percentage share of votes obtained by each party.
#' @param s A numeric vector with the percentage share of seats obtained by each party.
#' @param \dots Additional arguements (currently ignored)
#'
#' @details The score is calculated as 1 minus the sum of the absolute differences \code{|Z-R|} divided by the number of parties.
#' @return A single score.
#'
#' @author Daniel Marcelino \email{dmarcelino@@live.com}
#'
#' @seealso  \code{\link{Rae}}, \code{\link{CoxShugart}}, \code{\link{PoliticalDiversity}}, \code{\link{Grofman}}, \code{\link{Farina}},  \code{\link{Lijphart}}. For more details, see the Indices vignette: \code{vignette("Indices", package = "SciencesPo")}
#'
#' @references
#' Rae, D. (1967) \emph{The Political Consequences of Electoral Laws.} London: Yale University Press.
#'
#'  @examples
#' # 2012 Queensland state elecion
#' pvotes= c(49.65, 26.66, 11.5, 7.53, 3.16, 1.47)
#' pseats = c(87.64, 7.87, 2.25, 0.00, 2.25, 0.00)
#'
#' inv.Rae(pvotes, pseats)
#'
#' @export
#' @rdname inv.Rae
`inv.Rae`<- function(v, s, ...) UseMethod("inv.Rae")

#' @export
#' @rdname inv.Rae
`inv.Rae` <-function(v, s, ...){
  idx=1-(sum(abs(v-s))/length(v))
  print(idx, digits = max(3, getOption("digits") - 3))
}### end -- inverse rae function
NULL



#' @title Gallagher Index
#'
#' @description Calculates the Gallagher index (LSq index).
#'
#' @param v A numeric vector with the percentage share of votes obtained by each party.
#' @param s A numeric vector with the percentage share of seats obtained by each party.
#' @param \dots Additional arguements (currently ignored)
#'
#' @details The score is calculated as: sqrt(sum((v_{i}-s{i})^2)/2).
#' @return A single score.
#' @author Daniel Marcelino \email{dmarcelino@@live.com}
#'
#' @seealso \code{\link{CoxShugart}}, \code{\link{PoliticalDiversity}}, \code{\link{Grofman}}, \code{\link{Farina}},  \code{\link{Lijphart}}. For more details see the Indices vignette: \code{vignette("Indices", package = "SciencesPo")}
#'
#' @references
#'  Gallagher, M. (1991) Proportionality, disproportionality and electoral systems. Electoral Studies 10(1):33-51.
#'
#'  @examples
#' # 2012 Queensland state elecion
#' pvotes= c(49.65, 26.66, 11.5, 7.53, 3.16, 1.47)
#' pseats = c(87.64, 7.87, 2.25, 0.00, 2.25, 0.00)
#'
#' Gallagher(pvotes, pseats)

#' @export
#' @rdname Gallagher
`Gallagher`<- function(v, s, ...) UseMethod("Gallagher")

#' @export
#' @rdname Gallagher
`Gallagher` <-function(v, s, ...){
  idx=sqrt(sum((v-s)^2)/2)
  print(idx, digits = max(3, getOption("digits") - 3))
}### end -- gallagher function
NULL





#' @title Inverse of Gallagher Index
#'
#' @description Calculates the inverted Gallagher index or 1 - the Gallagher index.
#'
#' @param v A numeric vector with the percentage share of votes obtained by each party.
#' @param s A numeric vector with the percentage share of seats obtained by each party.
#' @param \dots Additional arguements (currently ignored)

#' @details The score is calculated as: 1 - sqrt(sum((v_{i}-s{i})^2)/2).
#' @return A single score.
#' @author Daniel Marcelino \email{dmarcelino@@live.com}
#'
#' @examples
#'
#' # 2012 Queensland state elecion:
#' pvotes= c(49.65, 26.66, 11.5, 7.53, 3.16, 1.47)
#' pseats = c(87.64, 7.87, 2.25, 0.00, 2.25, 0.00)
#'
#' inv.Gallagher(pvotes, pseats)
#'
#' @seealso \code{\link{CoxShugart}}, \code{\link{Farina}}, \code{\link{PoliticalDiversity}}, \code{\link{Grofman}}, \code{\link{Gallagher}},  \code{\link{Lijphart}}. For more details see the Indices vignette: \code{vignette("Indices", package = "SciencesPo")}.
#'
#' @export
#' @rdname inv.Gallagher
`inv.Gallagher` <-function(v, s, ...) UseMethod("inv.Gallagher")


#' @export
#' @rdname inv.Gallagher
`inv.Gallagher` <-function(v, s, ...){
  V <- mean(v)
  S <- mean(s)
  idx <- sum((v-V) * (s-S))/sum((s-S)^2)
  idx <- (1 - idx)
  print(idx, digits = max(3, getOption("digits") - 3))
}### end -- inv.gallagher function
NULL



#' @title Lijphart Index of Proportionality
#'
#' @description Calculates the Lijphart index of proportionality based on a vector of votes and a vector for the electoral outcome.
#'
#' @param v A numeric vector with the percentage share of votes obtained by each party.
#' @param s A numeric vector with the percentage share of seats obtained by each party.
#' @param \dots Additional arguements (currently ignored)
#'
#' @return A single score.
#' @author Daniel Marcelino \email{dmarcelino@@live.com}
#'
#' @seealso \code{\link{CoxShugart}}, \code{\link{PoliticalDiversity}}, \code{\link{Grofman}}, \code{\link{Gallagher}},  \code{\link{Farina}}. For more details see the Indices vignette: \code{vignette("Indices", package = "SciencesPo")}
#'
#' @examples

#' # 2012 Quebec provincial election:
#' pvotes = c(PQ=31.95, Lib=31.20, CAQ=27.05,QS=6.03,Option=1.89, Other=1.88)
#' pseats = c(PQ=54, Lib=50, CAQ=19, QS=2, Option=0, Other=0)
#'
#' Lijphart(pvotes, pseats)
#'
#' @rdname Lijphart
#' @export
`Lijphart`<- function(v, s, ...) UseMethod("Lijphart")


#' @export
#' @rdname Lijphart
`Lijphart` <-function(v, s, ...){
  idx=max(s-v)
  print(idx, digits = max(3, getOption("digits") - 3))
}### end -- lijphart function
NULL




#' @title Grofman Index
#'
#' @description Calculates the Grofman index of proportionality based on a vector of votes and a vector for the electoral outcome.
#'
#' @param v A numeric vector with the percentage share of votes obtained by each party.
#' @param s A numeric vector with the percentage share of seats obtained by each party.
#' @param \dots Additional arguements (currently ignored)
#'
#' @return A single score.
#'  @references
#' Taagepera, R., and B. Grofman. Mapping the indices of seats-votes disproportionality and inter-election volatility. Party Politics 9, no. 6 (2003): 659-77.
#'
#' @seealso \code{\link{CoxShugart}}, \code{\link{PoliticalDiversity}}, \code{\link{Farina}}, \code{\link{Gallagher}},  \code{\link{Lijphart}}. For more details see the Indices vignette: \code{vignette("Indices", package = "SciencesPo")}
#'
#' @author Daniel Marcelino \email{dmarcelino@@live.com}
#' @examples
#'
#' # 2012 Quebec provincial election:
#' pvotes = c(PQ=31.95, Lib=31.20, CAQ=27.05,QS=6.03,Option=1.89, Other=1.88)
#' pseats = c(PQ=54, Lib=50, CAQ=19, QS=2, Option=0, Other=0)
#'
#' Grofman(pvotes, pseats)
#'
#' @export
#' @rdname Grofman
`Grofman`<- function(v, s, ...) UseMethod("Grofman")

#' @rdname Grofman
#' @export
`Grofman` <- function(v, s, ...){
  N <- PoliticalDiversity(s, index = "laakso/taagepera")
  idx=(1/N) * sum(abs(v-s))/2
  print(idx, digits = max(3, getOption("digits") - 3))
}### end -- grofman function
NULL



#' @title Farina Index
#'
#' @description Calculates the Farina index also referred to as the cosine proportionality score based on a vector of votes and a vector for the electoral outcome.
#' @param v A numeric vector with the percentage share of votes obtained by each party.
#' @param s A numeric vector with the percentage share of seats obtained by each party.
#' @param \dots Additional arguements (currently ignored)
#'
#' @return A single score.
#' @seealso \code{\link{CoxShugart}}, \code{\link{PoliticalDiversity}}, \code{\link{Grofman}}, \code{\link{Gallagher}},  \code{\link{Lijphart}}. For more details see the Indices vignette: \code{vignette("Indices", package = "SciencesPo")}.
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
#' Farina(pvotes, pseats)
#'
#' @export Farina
#' @rdname Farina
`Farina`<- function(v, s, ...) UseMethod("Farina")

#' @export
#' @rdname Farina
`Farina` <- function(v, s, ...){
  idx= acos(sum(v*s)/(sum(v^2)*sum(s^2))^.5)
  print(idx, digits = max(3, getOption("digits") - 3))
}### end -- farina function
NULL





#' @title Cox-Shugart Measure of Proportionality
#'
#' @description Calculate the Cox and Shugart measure of
#'  proportionalitybased on a vector of votes and a vector for
#'  the electoral outcome. This measure is also referred to as the regression index.
#'
#' @param v A numeric vector with the percentage share of votes obtained by each party.
#' @param s A numeric vector with the percentage share of seats obtained by each party.
#' @param \dots Additional arguements (currently ignored)

#'
#' @return A single score.
#'
#' @author Daniel Marcelino \email{dmarcelino@@live.com}
#'
#' @seealso \code{\link{Farina}}, \code{\link{PoliticalDiversity}}, \code{\link{Grofman}}, \code{\link{Gallagher}},  \code{\link{Lijphart}}. For more details see the Indices vignette: \code{vignette("Indices", package = "SciencesPo")}.
#'
#' @examples
#' if (interactive()) {
#' # 2012 Queensland state elecion:
#' pvotes= c(49.65, 26.66, 11.5, 7.53, 3.16, 1.47)
#' pseats = c(87.64, 7.87, 2.25, 0.00, 2.25, 0.00)
#'
#' CoxShugart(pvotes, pseats)
#' }
#'
#' @export
#' @rdname CoxShugart
`CoxShugart` <- function(v, s, ...) UseMethod("CoxShugart")


#' @rdname CoxShugart
`CoxShugart` <- function(v, s, ...){
  S <- mean(s)
  V <- mean(v)
  idx <- sum((s-S) * (v-V))/sum((v-V)^2)
  print(idx, digits = max(3, getOption("digits") - 3))
}### end -- CoxShugart function
NULL



#' @title Inverse Cox-Shugart Measure of Proportionality
#'
#' @description Calculate the inverse Cox and Shugart measure of
#'  proportionality based on votes and seats,
#'  the electoral outcome.
#'
#' @param v A numeric vector with the percentage share of votes obtained by each party.
#' @param s A numeric vector with the percentage share of seats obtained by each party.
#' @param \dots Additional arguements (currently ignored)
#'
#' @return A single score.
#' @author Daniel Marcelino \email{dmarcelino@@live.com}
#'
#' @examples
#'
#' # 2012 Queensland state elecion:
#' pvotes= c(49.65, 26.66, 11.5, 7.53, 3.16, 1.47)
#' pseats = c(87.64, 7.87, 2.25, 0.00, 2.25, 0.00)
#'
#' inv.CoxShugart(pvotes, pseats)
#'
#' @seealso \code{\link{CoxShugart}}, \code{\link{Farina}}, \code{\link{PoliticalDiversity}}, \code{\link{Grofman}}, \code{\link{Gallagher}},  \code{\link{Lijphart}}. For more details see the Indices vignette: \code{vignette("Indices", package = "SciencesPo")}.
#'
#' @export
#' @rdname inv.CoxShugart
`inv.CoxShugart` <-function(v, s, ...) UseMethod("inv.CoxShugart")


#' @export
#' @rdname inv.CoxShugart
`inv.CoxShugart` <-function(v, s, ...){
  V <- mean(v)
  S <- mean(s)
  idx <- sum((v-V) * (s-S))/sum((s-S)^2)
  print(idx, digits = max(3, getOption("digits") - 3))
}### end -- inv.cox.shugart function
NULL




#' @title Atkinson Index of Inequality
#'
#' @description Calculates the Atkinson Index. This inequality measure is espcially good at determining which end of the distribution is contributing most to the observed inequality.
#'
#' @param x A vector of data values of non-negative elements.
#' @param n A vector of frequencies of the same length as \code{x}.
#' @param parameter A parameter of the inequality measure (if set to \code{NULL} the default parameter of the respective measure is used).
#' @param na.rm A logical. Should missing values be removed? The Default is set to \code{na.rm=FALSE}.
#' @param \dots Additional arguements (currently ignored)
#'
#' @references
#' Cowell, F. A. (2000) Measurement of Inequality in Atkinson, A. B. / Bourguignon, F. (Eds): \emph{Handbook of Income Distribution}. Amsterdam.
#'
#' Cowell, F. A. (1995) \emph{Measuring Inequality} Harvester Wheatshef: Prentice Hall.
#'
#' @seealso \code{\link{Herfindahl}}, \code{\link{Rosenbluth}},  \code{\link{Gini}}. For more details see the Indices vignette: \code{vignette("Indices", package = "SciencesPo")}.
#' @examples
#' if (interactive()) {
#' # generate a vector (of incomes)
#' x <- c(778, 815, 857, 888, 925, 930, 965, 990, 1012)
#'
#' # compute Atkinson coefficient with parameter=0.5
#' Atkinson(x, parameter=0.5)
#'}
#' @export
#' @rdname Atkinson
`Atkinson` <-function(x, n = rep(1, length(x)), parameter=0.5, na.rm=FALSE, ...) UseMethod("Atkinson")
NULL

#' @export
#' @rdname Atkinson
`Atkinson` <- function(x, n = rep(1, length(x)), parameter = 0.5, na.rm = FALSE, ...){
  x <- rep(x, n)    # same handling as Lc and Gini
  if(na.rm) x <- na.omit(x)
  if (any(is.na(x)) || any(x < 0)) return(NA_real_)

  if(is.null(parameter)) parameter <- 0.5
  if(parameter==1)
    idx <- 1 - (exp(mean(log(x)))/mean(x))
  else
  {
    x <- (x/mean(x))^(1-parameter)
    idx <- 1 - mean(x)^(1/(1-parameter))
  }
  print(idx, digits = max(3, getOption("digits") - 3))
}### end -- Atkinson function
NULL






#' @title Rosenbluth Index of Concentration
#'
#' @description Calculates the Rosenbluth Index of concentration, also known as Hall or Tiedemann Indices.
#'
#' @param x A vector of data values of non-negative elements.
#' @param n A vector of frequencies of the same length as \code{x}.
#' @param na.rm A logical. Should missing values be removed? The Default is set to \code{na.rm=FALSE}.
#' @param \dots Additional arguements (currently ignored)
#'
#' @references
#' Cowell, F. A. (2000) Measurement of Inequality in Atkinson, A. B. / Bourguignon, F. (Eds): \emph{Handbook of Income Distribution}. Amsterdam.
#'
#' Cowell, F. A. (1995) \emph{Measuring Inequality} Harvester Wheatshef: Prentice Hall.
#'
#' @seealso \code{\link{Atkinson}}, \code{\link{Herfindahl}},  \code{\link{Gini}}. For more details see the Indices vignette: \code{vignette("Indices", package = "SciencesPo")}.
#'
#' @examples
#' # generate a vector (of incomes)
#' x <- c(778, 815, 857, 888, 925, 930, 965, 990, 1012)
#'
#' # compute Rosenbluth coefficient
#' Rosenbluth(x)
#'
#' @export
#' @rdname Rosenbluth
`Rosenbluth` <-function(x, n = rep(1, length(x)), na.rm=FALSE, ...)  UseMethod("Rosenbluth")
NULL

#' @export
#' @rdname Rosenbluth
`Rosenbluth` <-function(x, n = rep(1, length(x)), na.rm = FALSE, ...){
  x <- rep(x, n)
  if(na.rm) x <- na.omit(x)
  if (any(is.na(x)) || any(x < 0)) return(NA_real_)
  n <- length(x)
  x <- sort(x)
  idx <- (n:1)*x
  idx <- 2*sum(idx/sum(x))
  idx <- 1/(idx-1)
  print(idx, digits = max(3, getOption("digits") - 3))
}### end -- Rosenbluth function
NULL









#' @encoding UTF-8
#' @title Herfindahl Index of Concentration
#'
#' @description Calculates the Herfindahl Index of concentration.
#'
#' @param x A vector of data values of non-negative elements.
#' @param n A vector of frequencies of the same length as \code{x}.
#' @param parameter A parameter of the concentration measure (if set to \code{NULL} the default parameter of the respective measure is used).
#' @param na.rm A logical. Should missing values be removed? The Default is set to \code{na.rm=FALSE}.
#' @param \dots Additional arguements (currently ignored)
#'
#' @details This index is also known as the \emph{Simpson Index} in ecology, the \emph{Herfindahl-Hirschman Index (HHI)} in economics, and as the \emph{Effective Number of Parties (ENP)} in political science.
#'
#' @references
#' Cowell, F. A. (2000) Measurement of Inequality in Atkinson, A. B. / Bourguignon, F. (Eds): \emph{Handbook of Income Distribution}. Amsterdam.
#'
#' Cowell, F. A. (1995) \emph{Measuring Inequality} Harvester Wheatshef: Prentice Hall.
#'
#' @seealso \code{\link{Atkinson}}, \code{\link{Rosenbluth}}, \code{\link{PoliticalDiversity}}, \code{\link{Gini}}. For more details see the Indices vignette: \code{vignette("Indices", package = "SciencesPo")}.
#' @examples
#' # generate a vector (of incomes)
#' x <- c(778, 815, 857, 888, 925, 930, 965, 990, 1012)
#'
#' # compute the Herfindahl coefficient with parameter=1
#' Herfindahl(x, parameter=1)
#'
#'
#' @export
#' @rdname Herfindahl
`Herfindahl` <- function(x, n = rep(1, length(x)), parameter = 1, na.rm = FALSE, ...) UseMethod("Herfindahl")


#' @export
#' @rdname Herfindahl
`Herfindahl` <- function(x, n = rep(1, length(x)), parameter = 1, na.rm = FALSE, ...){
  x <- rep(x, n)
  if(na.rm) x <- na.omit(x)
  if (any(is.na(x)) || any(x < 0)) return(NA_real_)

  if(is.null(parameter))
    m <- 1
  else
    m <- parameter
  idx <- x/sum(x)
  idx <- idx^(m+1)
  idx <- sum(idx)^(1/m)
  print(idx, digits = max(3, getOption("digits") - 3))
}### end -- herfindahl function
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
  names(.temp) <-c("Party", "Seats", "\u0025Seats");
  print(.temp, digits = max(3, getOption("digits") - 3))
}
NULL





#' @encoding UTF-8
#' @title The D'Hondt Method of Allocating Seats Proportionally
#'
#' @description The function calculate the seats allotment in legislative house, given the total number of seats and the votes for each party based on the Victor D'Hondt's method (1878), which is mathematically equivalent to the method proposed by Thomas Jefferson few years before (1792).
#'
#' @param parties A vector containig parties labels or candidates accordingly to the \code{votes} vector order.
#' @param votes A vector containing the total number of formal votes received by the parties/candidates.
#' @param seats An integer for the number of seats to be filled (the district magnitude).
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
#' # Example: 2014 Brazilian election for the lower house in
#' # the state of Ceara. Coalitions were leading by the
#' # following parties:
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
  out <- with(.temp, (parties[order(-scores)][1:seats]))
  out <- freq(out, digits = 3);
  names(out) <-c("Party", "Seats", "\u0025Seats");
  # out <- out[ order(out[,2], decreasing = TRUE),]
  return(out)
}
NULL




#' @encoding latin1
#' @title Highest Averages Methods of Allocating Seats Proportionally
#'
#' @description Computes the highest averages method for a variety of formulas of allocating seats proportionally.
#' @param parties A character vector for parties labels or candidates in the same order as \code{votes}. If \code{NULL}, alphabet will be assigned.
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
#' \item {"dh"}{d'Hondt method}
#' \item {"sl"}{Sainte-Lague method}
#' \item {"msl"}{Modified Sainte-Lague method}
#' \item {"danish"}{Danish modified Sainte-Lague method}
#' \item {"hsl"}{Hungarian modified Sainte-Lague method}
#' \item {"imperiali"}{The Italian Imperiali (not to be confused with the Imperiali quota which is a Largest remainder method)}
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
#' @seealso \code{\link{LargestRemainders}}, \code{\link{dHondt}}, \code{\link{Hamilton}}, \code{\link{PoliticalDiversity}}. For more details see the \emph{Indices} vignette: \code{vignette('Indices', package = 'SciencesPo')}.
#'
#' @examples
#' # Results for the state legislative house of Ceara (2014):
#' votes <- c(187906, 326841, 132531, 981096, 2043217, 15061, 103679,109830, 213988, 67145, 278267)
#'
#' parties <- c("PCdoB", "PDT", "PEN", "PMDB", "PRB", "PSB", "PSC", "PSTU", "PTdoB", "PTC", "PTN")
#'
#' HighestAverages(parties, votes, seats = 42, method = "dh")
#'
#' # Let's create a data.frame with typical election results
#' # with the following parties and votes to return 10 seats:
#'
#' my_election <- data.frame(
#' party=c("Yellow", "White", "Red", "Green", "Blue", "Pink"),
#' votes=c(47000, 16000,	15900,	12000,	6000,	3100))
#'
#' HighestAverages(my_election$party,
#' my_election$votes,
#' seats = 10,
#' method="dh")
#'
#' # How this compares to the Sainte-Lague Method
#'
#'(dat= HighestAverages(my_election$party,
#' my_election$votes,
#' seats = 10,
#' method="sl"))
#'
#' # Plot it
#' bar.plot(data=dat, "Party", "Seats") +
#' theme_fte()
#'
#' @rdname HighestAverages
#' @export
`HighestAverages` <- function(parties=NULL, votes=NULL, seats=NULL, method=c("dh", "sl", "msl", "danish", "hsl", "hh", "imperiali", "wb", "jef", "ad", "hb"), threshold=0, ...) UseMethod("HighestAverages")



#' @export
#' @rdname HighestAverages
`HighestAverages.default` <- function(parties=NULL, votes=NULL, seats=NULL, method=c("dh", "sl", "msl", "danish", "hsl", "hh", "imperiali", "wb", "jef", "ad", "hb"), threshold=0, ...){
  # Modified :
  # v0.0 2012-07-12
  # v0.0 2013-11-21
  # v0.2 2014-10-02
  # v0.3 2016-01-13
  # v0.3 2016-05-15
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
         dh = { #d'Hondt
           divisor.vec <- seq(from = 1, by = 1, length.out = seats)
           method.name <- c("d'Hondt")
         },
         sl = { #Sainte-Lague
           divisor.vec <- seq(from = 1, by = 2, length.out = seats)
           method.name <- c("Sainte-Lagu\u00EB")
         },
         msl = { #Modified Sainte-Lague
           divisor.vec <- c(1.4, seq(from = 3, by = 2, length.out = seats-1))
           method.name <- c("Modified Sainte-Lagu\u00EB")
         },
         danish = { #Danish
           divisor.vec <- c(1, seq(from = 4, by = 3, length.out = seats-1))
           method.name <- c("Danish Sainte-Lagu\u00EB")
         },
         hsl = { #Hungarian
           divisor.vec <- c(1.5, seq(from = 3, by = 2, length.out = seats-1))
           method.name <- c("Hungarian Sainte-Lagu\u00EB")
         },
         imperiali = { #Imperiali
           divisor.vec <- c(1, seq(from = 1.5, by = .5, length.out = seats-1))
           method.name <- c("Imperiali")
         },
         hh = { #Huntington-Hill Equal Proportions Method
           divisor.vec0 <- seq(from = 1, by = 1, length.out = seats)
           divisor.vec <- sqrt(divisor.vec0 * (divisor.vec0 - 1))
           method.name <- c("Hungtinton-Hill")
         },
         wb = { #Webster Major Fractions Method
           divisor.vec0 <- seq(from = 1, by = 2, length.out = seats)
           divisor.vec <- (divisor.vec0+(divisor.vec0 - 1))/2
           method.name <- c("Webster")
         },
         jef = { #Jefferson Greatest Divisors or Hagenbach-Bischoff Method
           divisor.vec <- seq(from = 1, by = 1, length.out = seats)
           method.name <- c("Jefferson")
         },
         ad = { #Adam's Method Smallest Devisors
           divisor.vec <- c(0, seq(from = 1, by = 1, length.out = seats-1))
           method.name <- c("Adam's Method")
         },
         hb = { #Hagenbach-Bischoff Method
           divisor.vec <- seq(from = 1, by = 1, length.out = seats)
           method.name <- c("Hagenbach-Bischoff")
         }
  )

  # ratio = as.vector(sapply(votes, function(x) x /
  # sum(votes)))
  .temp <- data.frame(
    parties = rep(parties, each = seats ),
    scores = as.vector(sapply(.votes, function(x) x /
                                divisor.vec ))
  );

  out <- with(.temp, (parties[order(-scores)][1:seats]))

  out <- freq(out, digits = 3);
  names(out) <-c("Party", "Seats", "\u0025Seats");
  # Political diversity indices
  ENP.votes <- 1/sum(.ratio^2)
  ENP.seats <- 1/sum((out$Seats/sum(out$Seats))^2)
  LSq.index <- sqrt(0.5*sum((((votes/sum(votes))*100) - ((out$Seats/sum(out$Seats))*100))^2))

  cat("Method:", method.name, "\n")
  shorten(round(divisor.vec, 2), 4)
  cat(paste("ENP:",round(ENP.votes,2),"(After):",round(ENP.seats,2)),"\n")
  cat(paste("Gallagher Index: ", round(LSq.index, 2)), "\n \n")
  return(out)
}
NULL








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
#' \item {"imperiali-quota"}{Imperiali quota}
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
#' @seealso  \code{\link{HighestAverages}}, \code{\link{dHondt}}, \code{\link{Hamilton}}, \code{\link{PoliticalDiversity}}. For more details see the \emph{Indices} vignette: \code{vignette('Indices', package = 'SciencesPo')}.
#'
#' @examples
#' # Let's create a data.frame with typical election results
#' # with the following parties and votes to return 10 seats:
#'
#' my_election <- data.frame(
#' party=c("Yellow", "White", "Red", "Green", "Blue", "Pink"),
#' votes=c(47000, 16000,	15900,	12000,	6000,	3100))
#'
#' LargestRemainders(my_election$party,
#' my_election$votes, seats = 10,  method="droop")
#'
#' LargestRemainders(my_election$party,
#' my_election$votes, seats = 10,  method="hare")
#'
#' @rdname LargestRemainders
#' @export
`LargestRemainders` <- function(parties=NULL, votes=NULL, seats=NULL, method=c("hare", "droop", "imperiali-quota"), threshold=0, ...) UseMethod("LargestRemainders")



#' @export
#' @rdname LargestRemainders
`LargestRemainders.default` <- function(parties=NULL, votes=NULL, seats=NULL, method=c("hare", "droop", "imperiali-quota"), threshold=0, ...){
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
           divisor.vec <- (sum(votes)/seats)
           method.name <- c("Hare")
         },
         droop = { #Droop
           divisor.vec <- (1 + (sum(votes)/(seats+1)))
           method.name <- c("Droop")
         },
         imperiali = { #Imperiali-Quota
           divisor.vec <- (sum(votes)/(seats + 2))
           method.name <- c("Imperiali-quota")
})

  base.seat <- votes%/%divisor.vec
  remain <- seats - sum(base.seat)

  .temp <- data.frame(
    parties = rep(parties, each = seats ),
    scores = as.vector(sapply(.votes, function(x) x /
                                divisor.vec ))
  );

}
NULL
