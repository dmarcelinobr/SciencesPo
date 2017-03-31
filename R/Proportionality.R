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
#' \item {"Mod.Gallagher" } { The Modified or Adjusted Gallagher index}
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
#' @seealso \code{\link{politicalDiversity}}, \code{\link{largestRemainders}}, \code{\link{highestAverages}}. For more details, see the Indices vignette: \code{vignette("Indices", package = "SciencesPo")}.

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
#' proportionality(pvotes, pseats) # default is Gallagher
#'
#' proportionality(pvotes, pseats, index="Rae")
#'
#' # proportionality(pvotes, pseats, index="Cox-Shugart")
#'
#' # 2012 Quebec provincial election:
#' pvotes = c(PQ=31.95, Lib=31.20, CAQ=27.05, QS=6.03, Option=1.89, Other=1.88)
#' pseats = c(PQ=54, Lib=50, CAQ=19, QS=2, Option=0, Other=0)
#'
#' proportionality(pvotes, pseats, index="Rae")
#'
#' @export
#' @docType methods
#' @aliases Disproportionality
`proportionality` <-
  function(v, s, index = "Gallagher", ...) {
    UseMethod("proportionality")
  }

#' @rdname proportionality
#' @aliases disproportionality
#' @export
`proportionality.default` <-
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
          "modgallagher",
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
    if (sum(v > 1, na.rm = TRUE)) {
      V <- (v / sum(v, na.rm = TRUE))
    }
    if (sum(s > 1, na.rm = TRUE)) {
      S <- (s / sum(s, na.rm = TRUE))
    }
    else {
      S <- s

      V <- v
    }

    switch(
      method,
      rae = {
        idx <- (sum(abs(V - S), na.rm = TRUE) / length(V))
        method.name <- c("Rae's Index")
      },
      invrae = {
        idx <- (sum(abs(V - S), na.rm = TRUE) / length(V))
        idx <- (1 - idx)
        method.name <- c("Rae's Index (inverse)")
      },
      # 1-(sum(v*s)/(sum(v^2)*sum(s^2))^.5) # 1 - cos
      loosemorehanby = {
        #Loosemore-Hanby
        idx <- (sum(abs(V - S), na.rm = TRUE) / 2)
        method.name <- c("Loosemore-Hanby's Index")
      },
      rose = {
        #Rose
        idx <- (sum(abs(V - S), na.rm = TRUE) / 2)
        idx <- (1 - idx)
        method.name <- c("Rose's Index")
      },
      gallagher = {
        #Gallagher
        idx <- sqrt(sum((V - S) ^ 2, na.rm = TRUE) / 2)
        method.name <- c("Gallagher's Index")
      },
      invgallagher = {
        #Inv-Gallagher
        idx <- sqrt(sum((V - S) ^ 2, na.rm = TRUE) / 2)
        idx <- (1 - idx)
        method.name <- c("Gallagher's Index (inverted)")
      },
      modgallagher = {
        # Modified Gallagher index or G'
        # (x'=x/sum(xi^2)^.5 ) part:
        v_div <- sum(V^2)^0.5
        v_mod <- V/v_div
        # (y'=y/sum(yi^2)^.5) part:
        s_div <- sum(S^2)^0.5
        s_mod <- S/s_div
        sqrt(sum((v_mod-s_mod)^2)/2)
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
        sum((S - mean(S, na.rm = TRUE)) * (V - mean(V, na.rm = TRUE)), na.rm = TRUE) / sum((V - mean(V, na.rm = TRUE)) ^ 2, na.rm = TRUE)
        method.name <- c("Cox-Shugart Index")
      },
      invcoxshugart = {
        #Cox-Shugart
        idx <-
          sum((V - mean(V, na.rm = TRUE)) * (S - mean(S, na.rm = TRUE))) / sum((S - mean(S, na.rm = TRUE)) ^ 2)
        method.name <- c("Cox-Shugart Index (inverted)")
      },
      farina = {
        #Farina
        idx = acos(sum(V * S, na.rm = TRUE) / (sum(V ^ 2, na.rm = TRUE) * sum(S ^ 2, na.rm = TRUE)) ^ .5)
        method.name <- c("Farina's Index")
      },
      lijphart = {
        #Lijphart
        # Transform to percent
        idx = max(S - V, na.rm = TRUE)
        method.name <- c("Lijphart's Index")
      },
      invlijphart = {
        idx = max(S - V, na.rm = TRUE)
        idx <- (1 - idx)
        method.name <- c("Lijphart's Index")
      },
      saintelague = {
        idx = sum((V - S)^2/S, na.rm = TRUE)
        method.name <- c("Sainte-Lague's Index")
      },
      invsaintelague = {
        idx = sum((S - V)^2/V, na.rm = TRUE)
        method.name <- c("Sainte-Lague's Index (inverted)")
      },
      dhondt = {
      idx = max(S/V, na.rm = TRUE)
      method.name <- c("D'Hondt Index")
      }
    )
    cat("\n")
    cat(method.name, ": ", round(idx, 3), "\n")
    invisible(idx)
  }### end -- proportionality function
NULL
