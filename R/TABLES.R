#' @encoding UTF-8
#' @title Cross-tabulation
#'
#' @description Makes a contingency table and computes chi-square test of independence, Phi coefficient, Cramer's V and Contingency coefficient.
#' @param .data The data.frame.
#' @param row The row variable.
#' @param col The column variable.
#' @param weights The frequency variable.
#' @param observed If FALSE, observed values are excluded from the table.
#' @param expected If TRUE, expected values are included.
#' @param sresid If TRUE, standardized residuals are included.
#' @param total.pct If TRUE, total proportions are included.
#' @param row.pct If TRUE, row proportions are included.
#' @param col.pct If TRUE, column proportions are included.
#' @param chisq If TRUE, the result of chi-square test of independence is included.
#' @param phi If TRUE, the Phi coefficient is included.
#' @param cramerV If TRUE, Cramer's V coefficient is included.
#' @param contingency If TRUE, the Contingency coefficient (C) is included.
#'
#' @examples
#' titanic %>% crosstabs("SEX", "AGE")
#'
#'
#' # Tea-Tasting Experiment data
#'  tea <- data.frame(
#'    expand.grid(poured=c("Yes", "No"),
#'    guess=c("Yes", "No")),
#'    count=c(3,1,1,3))
#'
#' tea %>% crosstabs("guess", "poured", "count", row.pct=TRUE, col.pct=TRUE)
#'
#' # Agresti (2002), table 3.10, p. 104
#' # 1991 General Social Survey: hypothesis of independence between
#' # party identification and race.
#' gss <- data.frame(
#'    expand.grid(race=c("black", "white"),
#'    party=c("dem", "indep", "rep")),
#'    count=c(103,341,15,105,11,405))
#'
#' crosstabs(gss, "race", "party", "count", chisq=TRUE)
#'
#' @export
`crosstabs` <- function(.data,
                        row,
                        col,
                        weights = NULL,
                        observed = TRUE,
                        expected = TRUE,
                        total.pct = FALSE,
                        row.pct = FALSE,
                        col.pct = FALSE,
                        sresid = FALSE,
                        chisq = FALSE,
                        phi = FALSE,
                        cramerV = FALSE,
                        contingency = FALSE) UseMethod("crosstabs")

#' @rdname crosstabs
#' @export
`crosstabs.default` <- function(.data,
                                row,
                                col,
                                weights = NULL,
                                observed = TRUE,
                                expected = TRUE,
                                total.pct = FALSE,
                                row.pct = FALSE,
                                col.pct = FALSE,
                                sresid = FALSE,
                                chisq = FALSE,
                                phi = FALSE,
                                cramerV = FALSE,
                                contingency = FALSE) {
  if (is.character(.data)) {
    nameData <- data.table::data.table(.data)
    if (!exists(.data)) { stop(paste("The object '", nameData,"' not found.", sep = "")) }
    tempData <- eval(parse(text = .data))
  } else {
    nameData <- paste(deparse(substitute(.data)))
    tempData <- .data
  }
  if (any(is.na(match(row, names(tempData))))) { stop("At least one item in the 'row' does not match any column in the dataset('", nameData,"').", sep = "")	}
  if (any(is.na(match(col, names(tempData))))) { stop("At least one item in the 'col' does not match any column in the dataset('", nameData,"').", sep = "")	}
  if (!is.null(weights)) {
    if(is.na(match(weights, names(tempData)))) { stop("At least one item in the 'col' does not match any column in the dataset('", nameData,"').", sep = "")     }
  }

  options(width = 5000, digits = 6)

  # determine the output needed
  show.stat <- NULL;
  RTHeading <- FALSE;
  CTHeading <- FALSE
  if (observed){ nobsFreq <- nchar("Obs Freq");
  RTHeading <- TRUE;
  CTHeading <- TRUE;
  show.stat <- c(show.stat, "observed")
  } else nobsFreq <- 0
  if (expected) {
    nexpFreq <- nchar("Exp Freq");
    RTHeading <- TRUE; CTHeading <- TRUE; show.stat <- c(show.stat, "expected")
  } else nexpFreq <- 0
  if (total.pct){
    ntotPercent <- nchar("% of Total");
    RTHeading <- TRUE;
    CTHeading <- TRUE;
    show.stat <- c(show.stat, "total.pct")
  } else ntotPercent <- 0
  if (row.pct) { RTHeading <- TRUE;
  show.stat <- c(show.stat, "row.pct")
  }
  if (col.pct) {
    CTHeading <- TRUE;
    show.stat <- c(show.stat, "col.pct")
  }

  if (!is.null(weights)) {
    row <- row[1]
    col <- col[1]
    tempTable <- matrix(0, nrow = length(unique(tempData[,row])), ncol = length(unique(tempData[,col])),
                        dimnames = list(unique(tempData[,row]),unique(tempData[,col])))
    for (i in (1:nrow(tempData))) {
      rowIndex <- match(tempData[i,row], dimnames(tempTable)[[1]])
      colIndex <- match(tempData[i,col], dimnames(tempTable)[[2]])
      tempTable[rowIndex, colIndex] <- tempTable[rowIndex, colIndex] + tempData[i,weights]
    }
  }
  crosstable <- NULL

  for (i in (1:length(row))) {
    for (j in (1:length(col))) {
      if (row[i] != col[j]) {
        if (!is.null(weights)) { crosstable <- tempTable } else{ crosstable <- table(tempData[,row[i]], tempData[,col[j]]) }
        if (row.pct) { nrowPercent <- nchar(paste("% within", row[i])) } else nrowPercent <- 0
        if (col.pct) { ncolPercent <- nchar(paste("% within", col[j])) }	else ncolPercent <- 0

        if (all(dim(crosstable) == 2)){
          result <- suppressWarnings(stats::chisq.test(crosstable, correct = FALSE))
        } else {
          result <- suppressWarnings(stats::chisq.test(crosstable, correct = FALSE)) }
        if (any(c(observed, expected, total.pct, row.pct, col.pct) == TRUE)) {
          # --- PRINTS SOME SUMMAY STATISTICS --- #
          rowPercentage <- prop.table(crosstable, 1) * 100
          colPercentage <- prop.table(crosstable, 2) * 100
          totPercentage <- prop.table(crosstable) * 100
          rowSum <- margin.table(crosstable,1)
          colSum <- margin.table(crosstable,2)

          RowHeading1 <- max(nchar(row[i]), nchar("Total"), nchar(dimnames(crosstable)[[1]])) + 1
          if (length(show.stat) == 1) { RowHeading2 <- 0; addSpace <- 1 } else { RowHeading2 <- max(nobsFreq, nexpFreq, ntotPercent, nrowPercent, ncolPercent) + 1; addSpace <- 2 }
          RowWidth = RowHeading1 + RowHeading2
          ColWidth <- max(nchar(crosstable), max(nchar(round(result[[7]], 0)), nchar(round(rowPercentage, 0)), nchar(round(colPercentage, 0)), nchar(round(totPercentage, 0))) + 2, nchar(dimnames(crosstable)[[2]]), nchar("Total")) + 2
          RowSpace1 <- paste(rep("-", RowWidth+addSpace), collapse = "")
          RowSpace2 <- paste(rep("-", ColWidth+1), collapse = "")
          show.numdigits <- max(nchar(round(rowPercentage,4)), nchar(round(colPercentage,4)), nchar(round(totPercentage,4)))

          options(width = 5000, digits = 6)
          if (length(show.stat) > 1) { cat("Table of ", row[i], " by ", col[j], "\n\n", sep = "")
          } else {
            if (observed) cat("Table of Observed Frequency: ", row[i], " by ", col[j], "\n\n", sep = "")
            if (expected) cat("Table of Expected Frequency: ", row[i], " by ", col[j], "\n\n", sep = "")
            if (total.pct) cat("Table of Total Percentage: ", row[i], " by ", col[j], "\n\n", sep = "")
            if (col.pct) cat("Table of Column Percentage: ", row[i], " by ", col[j], "\n\n", sep = "")
            if (row.pct) cat("Table of Row Percentage: ", row[i], " by ", col[j], "\n\n", sep = "")
          }
          cat(paste(formatC("", width = RowWidth + addSpace, format = "s", flag = "-"),"|", formatC(col[j], width = ColWidth, format = "s", flag = "-"), sep = "", collapse = "\n"), "\n")
          cat(RowSpace1, "+", sep = "")
          for (z in (1:ncol(crosstable))) cat(RowSpace2, "+", sep = "")
          if (RTHeading) cat(RowSpace2, "+", sep = "")
          cat("\n", formatC(row[i], width = RowWidth + addSpace, format = "s", flag = "-"), "|", sep = "")
          for (z in (1:ncol(crosstable))) cat(formatC(dimnames(crosstable)[[2]][z], width = ColWidth+1, format = "s", flag = "-"), "|", sep = "")
          if (observed || expected || total.pct || row.pct) {
            cat(formatC("Total", width = ColWidth+1, format = "s", flag = "-"), "|", "\n", sep = "")
          } else {
            cat("\n", sep = "")
          }
          cat(RowSpace1, "+", sep = "")
          for (z in (1:ncol(crosstable))) cat(RowSpace2, "+", sep = "")
          if (RTHeading) cat(RowSpace2, "+", sep = "")
          cat("\n")

          for (k in (1:(nrow(crosstable)+1))) {
            if (k <= nrow(crosstable)) { cat(formatC(dimnames(crosstable)[[1]][k], width = RowHeading1 + 1, format = "s", flag = "-"), "|", sep = "")
            } else { if (CTHeading) cat(formatC("Total", width = RowHeading1, format = "s", flag = "-"), "|") }
            if (observed) {
              if (k <= nrow(crosstable)) {
                if (length(show.stat) > 1) { cat(formatC("Obs Freq", width = RowHeading2, format = "s", flag = "-"), "|", sep = "") }
                for (z in (1:ncol(crosstable))) cat(formatC(crosstable[k,z], width = ColWidth+1, format = "d"), "|", sep = "")
                if (RTHeading) cat(formatC(rowSum[[k]], width = ColWidth+1, format = "d"), "|", "\n", sep = "")
              } else {
                if (length(show.stat) > 1) { cat(formatC("Obs Freq", width = RowHeading2, format = "s", flag = "-"), "|", sep = "") }
                for (z in (1:ncol(crosstable))) cat(formatC(colSum[[z]], width = ColWidth + 1, format = "d"), "|", sep = "")
                if (RTHeading) cat(formatC(sum(crosstable), width = ColWidth + 1, format = "d"), "|","\n", sep = "")
              }
            } ### end -- if (observed)

            if (expected) {
              if (k <= nrow(crosstable)) {
                if (show.stat[1] != "expected") { cat(formatC("", width = RowHeading1 + 1, format = "s"), "|", sep = "") }
                if (length(show.stat) > 1) { cat(formatC("Exp Freq", width = RowHeading2, format = "s", flag = "-"),"|", sep = "") }
                for (z in (1:ncol(crosstable))) cat(formatC(result[[7]][k,z], digits = 1, width = ColWidth + 1, format = "f", flag = "#"), "|", sep = "")
                if (RTHeading) cat(formatC(rowSums(result[[7]])[[k]], digits = 1, width = ColWidth + 1, format = "f", flag = "#"), "|", "\n", sep = "")
              } else {
                if (length(show.stat) > 1) {
                  cat(formatC("", width = RowHeading1 + 1, format = "s"), "|", sep = "")
                  cat(formatC("Exp Freq", width = RowHeading2, format = "s", flag = "-"), "|", sep = "")
                }
                for (z in (1:ncol(crosstable))) cat(formatC(colSums(result$exp)[[z]], digits = 1, width = ColWidth + 1, format = "f", flag = "#"), "|", sep = "")
                if (RTHeading) cat(formatC(sum(result$exp), digits = 1, width = ColWidth + 1, format = "f", flag = "#"), "|","\n", sep = "")
              }
            } ### end -- if (expected)

            if (row.pct && k <= nrow(crosstable)) {
              if (show.stat[1] != "row.pct") { cat(formatC("", width = RowHeading1 + 1, format = "s"), "|", sep = "") }
              if (length(show.stat) > 1) { cat(formatC(paste("% within", row[i]), width = RowHeading2, format = "s", flag = "-"), "|", sep = "") }
              for (z in (1:ncol(crosstable))) cat(formatC(rowPercentage[k,z], digits = 2, width = ColWidth + 1, format = "f", flag = "#"), "|", sep = "")
              if (RTHeading) cat(formatC(sum(rowPercentage[k,]), digits = 2, width = ColWidth + 1, format = "f", flag = "#"), "|", "\n", sep = "")
            } ### end -- if (row.pct && k <= nrow(crosstable))

            if (col.pct) {
              if (k <= nrow(crosstable)) {
                if (show.stat[1] != "col.pct") { cat(formatC("", width = RowHeading1 + 1, format = "s"), "|", sep = "") }
                if (length(show.stat) > 1) { cat(formatC(paste("% within", col[j]), width = RowHeading2, format = "s", flag = "-"),"|", sep = "") }
                for (z in (1:ncol(crosstable))) cat(formatC(colPercentage[k,z], digits = 2, width = ColWidth + 1, format = "f", flag = "#"),"|", sep = "")
                if (RTHeading) cat(formatC("", width = ColWidth + 1, format = "s"),"|", "\n",sep = "") else cat("\n", sep = "")
              } else {
                if (length(show.stat) > 1) {
                  cat(formatC("", width = RowHeading1 + 1, format = "s"), "|", sep = "")
                  cat(formatC(paste("% within", col[j]), width = RowHeading2, format = "s", flag = "-"),"|", sep = "")
                }
                for (z in (1:ncol(crosstable))) cat(formatC(colSums(colPercentage)[[z]], digits = 2, width = ColWidth + 1, format = "f", flag = "#"),"|", sep = "")
                if (RTHeading) { cat(formatC("", width = ColWidth + 1, format = "s"),"|", "\n",sep = "") } else { cat("\n", sep = "") }
              }
            } ### end -- if (col.pct)

            if (total.pct) {
              if (k <= nrow(crosstable)) {
                if (show.stat[1] != "total.pct") cat(formatC("", width = RowHeading1 + 1, format = "s"), "|", sep = "")
                if (length(show.stat) > 1) { cat(formatC("% of Total", width = RowHeading2, format = "s", flag = "-"), "|", sep = "") }
                for (z in (1:ncol(crosstable))) cat(formatC(totPercentage[k,z], digits = 2, width = ColWidth + 1, format = "f", flag = "#"), "|", sep = "")
                if (RTHeading) cat(formatC(rowSums(totPercentage)[[k]], digits = 2, width = ColWidth + 1, format = "f", flag = "#"), "|", "\n",sep = "")
              } else {
                if (length(show.stat) > 1) {
                  cat(formatC("", width = RowHeading1 + 1, format = "s"), "|", sep = "")
                  cat(formatC("% of Total", width = RowHeading2, format = "s", flag = "-"), "|", sep = "")
                }
                for (z in (1:ncol(crosstable))) cat(formatC(colSums(totPercentage)[[z]], digits = 2, width = ColWidth + 1, format = "f", flag = "#"), "|", sep = "")
                if (RTHeading) cat(formatC(sum(totPercentage), digits = 2, width = ColWidth + 1, format = "f", flag = "#"), "|", "\n",sep = "")
              }
            } ### end -- if (total.pct)
            if (CTHeading && k == (nrow(crosstable)+1)) {
              cat(RowSpace1, "+", sep = "")
              for (z in (1:ncol(crosstable))) cat(RowSpace2, "+", sep = "")
              if (RTHeading) cat(RowSpace2, "+", sep = "")
              cat("\n")
            }
            if (k <= nrow(crosstable)) {
              cat(RowSpace1, "+", sep = "")
              for (z in (1:ncol(crosstable))) cat(RowSpace2, "+", sep = "")
              if (RTHeading) cat(RowSpace2, "+", sep = "")
              cat("\n")
            }
          } ### for (k in (1:nrow(crosstable)))
          cat("\n")
        } ### if (any(c(observed, expected, total.pct, row.pct, col.pct) == TRUE))

        if (chisq || phi || cramerV || contingency) {
          chisq.label <- NULL
          if (chisq) {
            chisq.label <- c(chisq.label, "Pearson Chi-Square", "Likelihood Ratio Chi-Square")
            if (all(dim(crosstable) == 2)) { fisher.result <- suppressWarnings(stats::fisher.test(crosstable, alternative = "two.sided")); chisq.label <- c(chisq.label, "Fisher's Exact Test")	}
            tlable.length2 <- max(nchar(result[[2]][[1]]),2) + 2
            tlable.length3 <- nchar(round(result[[1]][[1]], 4)) + 2
          } else { tlable.length2 <- 0; tlable.length3 <- 0 }
          if (phi) 		{ chisq.label <- c(chisq.label, "Phi Coefficient") }
          if (contingency) 	{chisq.label <- c(chisq.label, "Contingency Coefficient") }
          if (cramerV) 	{ chisq.label <- c(chisq.label, "Cramer's V") }

          tlable.length1 <- max(nchar(chisq.label)) + 2
          ncell <- nrow(crosstable) * ncol(crosstable)
          NExpFreqless1 <- length(result[[7]][which(result[[7]] < 1)])
          NExpFreqless5 <- length(result[[7]][which(result[[7]] < 10)])
          cat("Statistics for Table ", row[i], " by ", col[j], "\n\n", sep = "")

          cat(formatC("Statistics", width = tlable.length1, format = "s", flag = "-"), sep = "")
          if (chisq) cat(formatC("DF", width = tlable.length2, format = "s", flag = "-"), sep = "")
          cat(formatC("Value", width = tlable.length3, format = "s"), sep = "")
          if (chisq) cat(formatC("Prob", width = 9, format = "s"), sep = "")
          cat("\n")

          cat(formatC(paste(rep("-", tlable.length1), collapse = ""), width = tlable.length1), sep = "")
          if (chisq) cat(formatC(paste(rep("-", tlable.length2), collapse = ""), width = tlable.length2), sep = "")
          cat(formatC(paste(rep("-", tlable.length3), collapse = ""), width = tlable.length3), sep = "")
          if (chisq) cat(formatC(paste(rep("-", 9), collapse = ""), width = 9), sep = "")
          cat("\n")

          if (chisq) {
            cat(formatC("Pearson Chi-Square", width = tlable.length1, format = "s", flag = "-"),
                formatC(result[[2]][[1]], width = tlable.length2, flag = "-"),
                formatC(result[[1]][[1]], digits = 3, width = tlable.length3, format = "f", flag = "#"),
                formatC(result[[3]][[1]], digits = 3, width = 8, format = "f", flag = "#"), "\n", sep = "")
            cat(formatC(chisq.label[2], width = tlable.length1, format = "s", flag = "-"),
                formatC(suppressWarnings(calc.LR(crosstable)$df), width = tlable.length2, flag = "-"),
                formatC(suppressWarnings(calc.LR(crosstable)$statistics), digits = 3, width = tlable.length3, format = "f", flag = "#"),
                formatC(suppressWarnings(calc.LR(crosstable)$p.value), digits = 3, width = 8, format = "f", flag = "#"), "\n", sep = "")
          }

          if (phi) {
            cat(formatC("Phi Coefficient", width = tlable.length1, format = "s", flag = "-"), sep = "")
            if (chisq) cat(formatC("", width = tlable.length2, format = "s", flag = "-"), sep = "")
            cat(formatC(suppressWarnings(calc.Phi(crosstable)), digits = 3, width = tlable.length3, format = "f", flag = "#"), "\n", sep = "")
          }
          if (contingency) {
            cat(formatC("Contingency Coefficient", width = tlable.length1, format = "s", flag = "-"), sep = "")
            if (chisq) cat(formatC("", width = tlable.length2, format = "s", flag = "-"), sep = "")
            cat(formatC(suppressWarnings(calc.CC(crosstable)), digits = 3, width = tlable.length3, format = "f", flag = "#"), "\n", sep = "")
          }
          if (cramerV) {
            cat(formatC("Cramer's V", width = tlable.length1, format = "s", flag = "-"), sep = "")
            if (chisq) cat(formatC("", width = tlable.length2, format = "s", flag = "-"), sep = "")
            cat(formatC(suppressWarnings(calc.CV(crosstable)), digits = 3, width = tlable.length3, format = "f", flag = "#"), "\n", sep = "")
          }
          if (all(dim(crosstable) == 2) && chisq) {
            cat(formatC("Fisher's Exact Test", width = tlable.length1, format = "s", flag = "-"),
                formatC("", width = tlable.length2, format = "s", flag = "-"),
                formatC("", width = tlable.length3, format = "s", flag = "-"),
                formatC(fisher.result[[1]], digits = 3, width = 8, format = "f", flag = "#"), "\n", sep = "")
          }
          cat(formatC(paste(rep("-", tlable.length1), collapse = ""), width = tlable.length1), sep = "")
          if (chisq) cat(formatC(paste(rep("-", tlable.length2), collapse = ""), width = tlable.length2), sep = "")
          cat(formatC(paste(rep("-", tlable.length3), collapse = ""), width = tlable.length3), sep = "")
          if (chisq) cat(formatC(paste(rep("-", 9), collapse = ""), width = 9), sep = "")
          cat("\n")

          num.error <- 0
          if (NExpFreqless5 > 0) { cat(paste(rep("*", num.error <- num.error + 1), collapse = "", sep = ""), " Cells with expected counts < 5: ", NExpFreqless5, " of ", ncell, " (",round((NExpFreqless5/ncell)*100, 2),"%)\n  Chi-Square may not be a valid test.", sep = "") }
          if (NExpFreqless1 > 0) { cat(paste(rep("*", num.error <- num.error + 1), collapse = "", sep = ""), " Cells with Expected counts < 1: ", NExpFreqless1, " of ", ncell, " (",round((NExpFreqless1/ncell)*100, 2),"%)\n  Chi-Square may not be a valid test.", sep = "") }
          cat("\n")
        } ### end -- if (chisq || phi || cramerV || contingency)

        if (sresid) {
          cat("Residuals for Table ", row[i], " by ", col[j], "\n\n", sep = "")
          cat(paste(formatC("", width = RowHeading1 + addSpace, format = "s", flag = "-"),"|", formatC(col[j], width = ColWidth, format = "s", flag = "-"), sep = "", collapse = "\n"), "\n")
          cat(paste(rep("-",RowHeading1+addSpace), collapse = ""), "+", sep = "")
          for (z in (1:ncol(crosstable))) cat(RowSpace2, "+", sep = "")
          cat("\n", formatC(row[i], width = RowHeading1 + addSpace, format = "s", flag = "-"), "|", sep = "")
          for (z in (1:ncol(crosstable))) cat(formatC(dimnames(crosstable)[[2]][z], width = ColWidth+1, format = "s", flag = "-"), "|", sep = "")
          cat("\n")
          cat(paste(rep("-", RowHeading1+addSpace), collapse = ""), "+", sep = "")
          for (z in (1:ncol(crosstable))) cat(RowSpace2, "+", sep = "")
          cat("\n")
          for (k in (1:nrow(crosstable))) {
            cat(formatC(dimnames(crosstable)[[1]][k], width = RowHeading1 + addSpace, format = "s", flag = "-"), "|",sep = "")
            for (z in (1:ncol(crosstable))) {
              cat(formatC(result[[8]][k,z], digits = 3, width = ColWidth + 1, format = "f", flag = "#"), "|",sep = "")
            }
            cat("\n")
          }
          cat(paste(rep("-", RowHeading1+addSpace), collapse = ""), "+", sep = "")
          for (z in (1:ncol(crosstable))) cat(RowSpace2, "+", sep = "")
          cat("\n\n")
        }
      } ### if (row[i] != col[j])
    } ## for (j in (1:length(col)))
  } ## for (i in (1:length(row)))
} ### end -- crossTable function
NULL


#' `tab` <- function(.data, row, col){
#'  argx <- deparse(substitute(row))
#'  argy <- deparse(substitute(col))
#'  return(crosstabs(.data, argx, argy,
#'                   row.pct=FALSE,
#'                   col.pct = FALSE,
#'                   total.pct = FALSE,
#'                   chisq = FALSE))
#'} ### end -- tab function
#'NULL







#' @encoding UTF-8
#' @title Stata-Like Two-Way Tabulation
#'
#' @description The function is a modified version of \code{\link{crosstabs}} for printing a summary table with cell counts and column proportions (similar to STATA's
#' \code{tabulate varname1 varname2, col}).
#'
#' @param x,y The variables for the cross tabulation.
#' @param digits The number of digits for rounding proportions.
#' @seealso \code{\link[stats]{xtabs}}, \code{\link{crosstabs}},
#' \code{\link[base]{table}}, \code{\link[base]{prop.table}}
#' @export
#' @examples
#' # Agresti (2002), table 3.10, p. 106
#' # 1992 General Social Survey- Race and Party affiliation
#' gss <- data.frame(
#'    expand.grid(Race=c("black", "white"),
#'    party=c("dem", "indep", "rep")),
#'    count=c(103,341,15,105,11,405))
#'
#' df <- gss[rep(1:nrow(gss), gss[["count"]]), ]
#' with(df, tab(Race, party))
#'
`tab` <- function(x, y, digits = 2){

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
NULL
