#' @title Indexes of (Disproportionality) Proportionality
#'
#' @description Calculates several indexes of (dis)-proportionality, which are for the most part used to show the relationship of votes to seats.
#'
#' @param v a numeric vector with the percentage share of votes obtained by each party.
#' @param s a numeric vector with the percentage share of seats obtained by each party.
#' @param index the desired method or type of index, see details below for the correct name.
#' @param margin The margin for which the index is computed.
#' @param \dots additional arguments (currently ignored)
#' @return Each iteration returns a single score.
#'
#' @author Daniel Marcelino \email{dmarcelino@@live.com}
#' @details The following measures are available:
#' \itemize{
#' \item {"Cox-Shugart"}{Cox-Shugart Measure of Proportionality}
#' \item {"Inv.Cox-Shugart"}{The Inverse of Cox-Shugart Index}
#' \item {"Farina"}{Farina Index of Proportionality}
#' \item {"Gallagher"}{Gallagher Index of Disproportionality}
#' \item {"Inv.Gallagher"}{The Inverse of Gallagher Index}
#' \item {"Grofman"}{Grofman Index of Proportionality}
#' \item {"Lijphart"}{Lijphart Index of Proportionality}
#' \item {"Loosemore-Hanby"}{Loosemore-Hanby Index of Disproportionality}
#' \item {"Rae"}{Rae Index of Disproportionality}
#' \item {"Inv.Rae"}{The Inverse of the Rae Index}
#' \item {"Rose"}{Rose Index of Disproportionality}
#' \item {"Inv.Rose"}{The Inverse of the Rose Index}
#' }
#'
#' @seealso \code{\link{PoliticalDiversity}}, \code{\link{LargestRemainders}}, \code{\link{HighestAverages}}. For more details, see the Indices vignette: \code{vignette("Indices", package = "SciencesPo")}.

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
`Proportionality.default` <- function(v, s, index = "Gallagher", margin = 1, ...){
  if (!is(v, "numeric") && !is(v, "integer")) {
    stop("\"v\" must be numeric")
  }
  else if (!is(s, "numeric") && !is(s, "integer")) {
    stop("\"v\" must be numeric")
  }

  v <- drop(as.matrix(v));
  s <- drop(as.matrix(s));

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
  method <- .Match(arg = index, choices = c("Cox-Shugart","Inv.Cox-Shugart","Farina","Gallagher","Inv.Gallagher", "Grofman", "Lijphart", "Loosemore-Hanby", "Rose", "Inv.Rose", "Rae","Inv.Rae") )

  if (method=="Gallagher")
    idx <- sqrt(sum((v-s)^2)/2)
  else if (method=="Inv.Gallagher"){
    V <- mean(v)
    S <- mean(s)
    idx <- sum((v-V) * (s-S))/sum((s-S)^2)
    idx <- (1 - idx)
  }
  else if (method=="Cox-Shugart"){
    S <- mean(s)
    V <- mean(v)
    idx <- sum((s-S) * (v-V))/sum((v-V)^2)
  }
  else if (method=="Inv.Cox-Shugart"){
    V <- mean(v)
    S <- mean(s)
    idx <- sum((v-V) * (s-S))/sum((s-S)^2)
  }
  else if (method == "Grofman"){
    N <- (1/sum((s/sum(s))^2) )
    idx=(1/N) * sum(abs(v-s))/2
  }
  else if (method == "Farina")
    idx= acos(sum(v*s)/(sum(v^2)*sum(s^2))^.5)
  else if (method == "Lijphart")
    idx=max(s-v)
  else if (method == "Rose")
    idx <- 1-(sum(abs(v-s))/2)
  else if (method=="Rae")
    idx <- (sum(abs(v - s))/length(v))
  else if (method=="Inv.Rae"){
    idx <- (sum(abs(v - s))/length(v))
    idx <- (1 - idx)
  }
  else if (method=="Loosemore-Hanby")
    idx <- (sum(abs(v-s))/2)
  else if (method=="Inv.Rae")
    idx <- (sum(abs(v - s))/length(v))
  else if (method=="Loosemore-Hanby")
    idx <- (sum(abs(v-s))/2)
  else
    warning(paste(index), " is not a valid index name. See `details` in the function documentation.")
  print(idx, digits = max(3, getOption("digits") - 3))
}### end -- proportionality function
NULL
