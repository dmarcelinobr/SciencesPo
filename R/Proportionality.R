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
#' \item {Loosemore-Hanby} {(Percent) Loosemore-Hanby Index of disproportionality}
#' \item {Rae} {(Percent) Rae Index of disproportionality}

#' \item {Cox-Shugart} { Cox-Shugart Index of proportionality}
#' \item {Inv.Cox-Shugart} { The inverted Cox-Shugart index}
#' \item {Farina} { Farina index of proportionality, aka cosine proportionality score}
#' \item {"Gallagher" } { (Percent) Gallagher index of disproportionality}
#' \item {"Inv.Gallagher" } { The inverse of Gallagher index}
#' \item {"Grofman" } { Grofman index of proportionality}
#' \item {"Inv.Grofman" } { Grofman index of proportionality}
#' \item {"Lijphart" } { Lijphart index of proportionality}
#' \item {"Inv.Rae" } { The inverse of Rae index}
#' \item {"Rose" } { Rose index of disproportionality}
#' \item {"Inv.Rose" } { The inverse of Rose index}
#' \item {"Sainte-Lague" } { Sainte-Lague index of disproportionality}
#' \item {"DHondt" } { D'Hondt index of proportionality}
#' \item {"Gini" } { Gini index of disproportionality}
#' \item {"Monroe" } { Monroe index of inequity}
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
#' Proportionality(pvotes, pseats) # default is Gallagher
#'
#' Proportionality(pvotes, pseats, index="Rae")
#'
#' # Proportionality(pvotes, pseats, index="Cox-Shugart")
#'
#' # 2012 Quebec provincial election:
#' pvotes = c(PQ=31.95, Lib=31.20, CAQ=27.05, QS=6.03, Option=1.89, Other=1.88)
#' pseats = c(PQ=54, Lib=50, CAQ=19, QS=2, Option=0, Other=0)
#'
#' Proportionality(pvotes, pseats, index="Rae")
#'
#' @export
#' @docType methods
#' @aliases Disproportionality
`Proportionality` <-
  function(v, s, index = "Gallagher", ...) {
    UseMethod("Proportionality")
  }

#' @rdname Proportionality
#' @aliases Disproportionality
#' @export
`Proportionality.default` <-
  function(v,
           s,
           index = "Gallagher",
           margin = 1,
           ...) {
index <- gsub("[^[:alnum:][:blank:]+?&/\\]", "", index)

    method <-
      .Match(
        arg = index,
        choices = c(
          "coxshugart",
          "invcoxshugart",
          "farina",
          "gallagher",
          "invgallagher",
          "grofman",
          "invgrofman",
          "lijphart",
          "invlijphart",
          "loosemorehanby",
          "rose",
          "invrose",
          "rae",
          "invrae",
          "saintelague",
          "invsaintelague",
          "dhondt",
          "monroe",
          "gini"
        )
      )

    v <- drop(as.matrix(v))
    s <- drop(as.matrix(s))

    if (length(dim(v)) > 1) {
      total_v <- apply(v, margin, sum)
      V <- sweep(v, margin, total_v, "/")
    }
    if (length(dim(s)) > 1) {
      total_s <- apply(s, margin, sum)
      S <- sweep(s, margin, total_s, "/")
    }

    if (Sum(v > 1)) {
      V <- (v / Sum(v))
    }
    if (Sum(s > 1)) {
      S <- (s / Sum(s))
    }
    else {
      S <- s

      V <- v

    }

    switch(
      method,
      rae = {
        idx <- (Sum(abs(V - S)) / length(V))
        method.name <- c("Rae's Index")
      },
      invrae = {
        idx <- (Sum(abs(V - S)) / length(V))
        idx <- (1 - idx)
        method.name <- c("Rae's Index (inverse)")
      },
      loosemorehanby = {
        #Loosemore-Hanby
        idx <- (Sum(abs(V - S)) / 2)
        method.name <- c("Loosemore-Hanby's Index")
      },
      rose = {
        #Rose
        idx <- (Sum(abs(V - S)) / 2)
        idx <- (1 - idx)
        method.name <- c("Rose's Index")
      },
      gallagher = {
        #Gallagher
        idx <- sqrt(Sum((V - S) ^ 2) / 2)
        method.name <- c("Gallagher's Index")
      },
      invgallagher = {
        #Inv-Gallagher
        idx <- sqrt(Sum((V - S) ^ 2) / 2)
        idx <- (1 - idx)
        method.name <- c("Gallagher's Index (inverted)")
      },
      grofman = {
        #Grofman
        # calculate the sum of the absolute differences |v-s|
        N <- (1 / sum(S ^ 2)) # neff_seats
        idx = (1 / N) * (sum(abs(V - S)) / 2)
        method.name <- c("Grofman's Index")
      },
      invgrofman = {
        N <- (1 / sum(S ^ 2)) # neff_seats
        idx = (1 / N) * (sum(abs(V - S)) / 2)
        method.name <- c("Grofman's Index (inverted)")
      },
      coxshugart = {
        #Cox-Shugart
        idx <-
        Sum((S - Mean(S)) * (V - Mean(V))) / Sum((V - Mean(V)) ^ 2)
        method.name <- c("Cox-Shugart Index")
      },
      invcoxshugart = {
        #Cox-Shugart
        idx <-
          sum((V - Mean(V)) * (S - Mean(S))) / sum((S - Mean(S)) ^ 2)
        method.name <- c("Cox-Shugart Index (inverted)")
      },
      farina = {
        #Farina
        idx = acos(Sum(V * S) / (Sum(V ^ 2) * Sum(S ^ 2)) ^ .5)
        method.name <- c("Farina's Index")
      },
      lijphart = {
        #Lijphart
        # Transform to percent
        idx = Max(S - V)
        method.name <- c("Lijphart's Index")
      },
      invlijphart = {
        idx = Max(S - V)
        idx <- (1 - idx)
        method.name <- c("Lijphart's Index")
      },
      saintelague = {
        idx = Sum((V - S)^2/S)
        method.name <- c("Sainte-Lague's Index")
      },
      invsaintelague = {
        idx = Sum((S - V)^2/V)
        method.name <- c("Sainte-Lague's Index (inverted)")
      },
      dhondt = {
      idx = Max(S/V)
      method.name <- c("D'Hondt Index")
      }
    )
    cat("\n")
    cat(method.name, ": ", round(idx, 3), "\n")
    invisible(idx)
  }### end -- proportionality function
NULL
