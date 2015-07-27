#' Tabulate
#'
#' \code{tabulate} is a modified version of \code{\link{crosstable}} for printing a summary table with cell counts and column proportions (similar to STATA's
#' \code{tabulate} \emph{varname1} \emph{varname2}, \code{col}).
#'
#' @param x,y The variables for the cross tabulation.
#' @param digits The number of digits for rounding proportions.
#' @seealso \code{\link[stats]{xtabs}}, \code{\link{crosstable}},
#' \code{\link[base]{table}}, \code{\link[base]{prop.table}}
#' @export
#' @examples
#' # Agresti (2002), table 3.11, p. 106
#' # 1992 General Social Survey- Sex and Party affiliation
#' gss <- data.frame(
#'    expand.grid(sex=c("female", "male"),
#'    party=c("dem", "indep", "rep")),
#'    count=c(279,165,73,47,225,191))
#'
#'  #Get it expanded
#' df <- gss[rep(1:nrow(gss), gss[["count"]]), ]
#'
#' with(df, tabulate(sex, party))
#'
#' # Agresti (2002), table 3.10, p. 106
#' # 1992 General Social Survey- Race and Party affiliation
#' gss <- data.frame(
#'    expand.grid(Race=c("black", "white"),
#'    party=c("dem", "indep", "rep")),
#'    count=c(103,341,15,105,11,405))
#'
#' df <- gss[rep(1:nrow(gss), gss[["count"]]), ]
#' with(df, tabulate(Race, party))
#'
tabulate <- function(x, y, digits = 2){

  dig <- digits
  xName <- deparse(substitute(x))
  yName <- deparse(substitute(y))
  x <- factor(x)
  y <- factor(y)
  tab <- table(x, y)
  dim1 <- dimnames(tab)[[1]]
  dim2 <- dimnames(tab)[[2]]

  # Calcs
  col_prop <- prop.table(tab, 2)
    tot_prop <- prop.table(tab)
  Obs <- sum(tab)
  row_sum <- rowSums(tab)
  col_sum <- colSums(tab)

  # Formatting Setup
  col_total.lab <- "TOTAL"
  row_total.lab <- "TOTAL"
  maxC <- max(2 + dig, nchar("TOTAL"), nchar(dim2), nchar(tab), nchar(row_sum), nchar(col_sum))
  maxR <- max(nchar(dim1), nchar("TOTAL"))
  col_total.lab <- formatC(col_total.lab, width = maxR, format = "s")
  row_total.lab <- formatC(row_total.lab, width = maxC, format = "s")
  Lines <- paste(rep("-", 2 + maxC), collapse = "")
  inner.Lines <- paste(rep("-", 1 + maxR), collapse = "")
  Rspaces <- paste(rep(" ", maxR), collapse = "")
  Cspaces <- paste(rep(" ", maxC), collapse = "")
  outer.Column <- formatC(dim1, width = maxR, format = "s")



  printTheTable <- function() {
    xyNames <- abbreviate(c(xName,yName), minlength = 5, dot = T)
    cat(rep(Rspaces, ncol(tab)), "[Y]", xyNames[2], collapse = "\n")
    cat("[X]", xyNames[1], collapse = "\n")
    cat(Rspaces,
        formatC(dim2, width = maxC, format = "s"),
        row_total.lab,
        sep = " | ",
        collapse = "\n"
    )
    cat(inner.Lines,
        rep(Lines, ncol(tab) + 1),
        sep = "|",
        collapse = "\n"
    )
    for (i in 1:nrow(tab)) {
      cat(outer.Column[i],
          formatC(c(tab[i, ], row_sum[i]), width = maxC, format = "d"),
          sep = " | ",
          collapse = "\n"
      )
      cat(Rspaces,
          formatC(c(col_prop[i, ],row_sum[i]/Obs), width = maxC, digits = dig, format = "f"),
          sep = " | ",
          collapse = "\n"
      )
      #       cat(Rspaces,
      #           formatC(c(  tot_prop[i, ],row_sum[i]/Obs), width = maxC, digits = dig, format = "f"),
      #           #         Cspaces,
      #           sep = " | ",
      #           collapse = "\n"
      #       )
      cat(inner.Lines,
          rep(Lines, ncol(tab) + 1),
          sep = "|",
          collapse = "\n"
      )
    }
    cat(col_total.lab,
        formatC(c(col_sum, Obs), width = maxC, format = "d"),
        sep = " | ",
        collapse = "\n"
    )
    #     cat(" Row%", formatC(c(col_sum/Obs, sum(row_sum/Obs)), width = maxC, digits = dig, format = "f"),
    #         #       Cspaces,
    #         sep = " | ",
    #         collapse = "\n"
    #     )
    #     cat(Rspaces,
    #         formatC(c(sum(col_prop[,1]),sum(col_prop[,2])), width = maxC, digits = dig, format = "f"),
    #         Cspaces,
    #         sep = " | ",
    #         collapse = "\n"
    #     )
    cat(Rspaces,
        formatC(rep(1, ncol(tab)), width = maxC, digits = dig, format = "f"),
        Cspaces,
        sep = " | ",
        collapse = "\n"
    )
    cat(inner.Lines,
        rep(Lines, ncol(tab) + 1),
        sep = "|",
        collapse = "\n"
    )
  }

  # Cell contents
  cat(rep("\n", 2))
  cat("          || Key || \n")
  cat("|=========================|\n")
  cat("|               Frequency |\n")
  cat("|       Column Proportion |\n")
  cat("|=========================|\n")
  cat("\n")
  cat("Total Observations in Table: ", Obs, "\n")
  cat("\n")
  cat("X = ", xName, "\n")
  cat("Y = ", yName, "\n")
  cat(rep("\n", 2))

  printTheTable()
}
