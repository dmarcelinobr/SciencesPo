#' @encoding UTF-8
#' @title Cross-tabulation
#'
#' @description Makes a contingency table and computes chi-square test of independence, phi coefficient, cramers v and contingency coefficient.
#' @param data The data.frame.
#' @param rowVar The row variable.
#' @param colVar The column variable.
#'
#' @export
Crosstab <- function(data,
                     rowVar,
                     colVar,
                     freqVar = NULL,
                     observed = TRUE,
                     expected = FALSE,
                     stdresid = FALSE,
                     totPercent = FALSE,
                     rowPercent = FALSE,
                     colPercent = FALSE,
                     chisq = FALSE,
                     phi = FALSE,
                     cramersv = FALSE,
                     contingency = FALSE) UseMethod("Crosstab")

Crosstab.default <- function(data,
                             rowVar,
                             colVar,
                             freqVar = NULL,
                             observed = TRUE,
                             expected = FALSE,
                             stdresid = FALSE,
                             totPercent = FALSE,
                             rowPercent = FALSE,
                             colPercent = FALSE,
                             chisq = FALSE,
                             phi = FALSE,
                             cramersv = FALSE,
                             contingency = FALSE) {
	if (is.character(data)) {
		nameData <- data
		if (!exists(data)) { stop(paste("The object '", nameData,"' not found.", sep = "")) }
		tempData <- eval(parse(text = data))
	} else {
		nameData <- paste(deparse(substitute(data)))
		tempData <- data
	}
	if (any(is.na(match(rowVar, names(tempData))))) { stop("At least one item in the 'rowVar' does not match any column in the dataset('", nameData,"').", sep = "")	}
	if (any(is.na(match(colVar, names(tempData))))) { stop("At least one item in the 'colVar' does not match any column in the dataset('", nameData,"').", sep = "")	}
	if (!is.null(freqVar)) {
          if(is.na(match(freqVar, names(tempData)))) { stop("At least one item in the 'colVar' does not match any column in the dataset('", nameData,"').", sep = "")     }
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
	  nexpFreq <- nchar("Expected Freq");
	  RTHeading <- TRUE; CTHeading <- TRUE; show.stat <- c(show.stat, "expected")
	  } else nexpFreq <- 0
	if (totPercent){
	  ntotPercent <- nchar("% of Total");
	  RTHeading <- TRUE;
	  CTHeading <- TRUE;
	  show.stat <- c(show.stat, "totPercent")
	  } else ntotPercent <- 0
	if (rowPercent) { RTHeading <- TRUE;
	show.stat <- c(show.stat, "rowPercent")
	}
	if (colPercent) {
	  CTHeading <- TRUE;
	  show.stat <- c(show.stat, "colPercent")
	  }

	if (!is.null(freqVar)) {
          rowVar <- rowVar[1]
          colVar <- colVar[1]
	     tempTable <- matrix(0, nrow = length(unique(tempData[,rowVar])), ncol = length(unique(tempData[,colVar])),
	                         dimnames = list(unique(tempData[,rowVar]),unique(tempData[,colVar])))
	     for (i in (1:nrow(tempData))) {
	          rowIndex <- match(tempData[i,rowVar], dimnames(tempTable)[[1]])
	          colIndex <- match(tempData[i,colVar], dimnames(tempTable)[[2]])
	          tempTable[rowIndex, colIndex] <- tempTable[rowIndex, colIndex] + tempData[i,freqVar]
	     }
	}
   crosstable <- NULL

	for (i in (1:length(rowVar))) {
		for (j in (1:length(colVar))) {
			if (rowVar[i] != colVar[j]) {
			     if (!is.null(freqVar)) { crosstable <- tempTable } else{ crosstable <- table(tempData[,rowVar[i]], tempData[,colVar[j]]) }
				if (rowPercent) { nrowPercent <- nchar(paste("% within", rowVar[i])) } 	else nrowPercent <- 0
				if (colPercent) { ncolPercent <- nchar(paste("% within", colVar[j])) } 	else ncolPercent <- 0

				if (all(dim(crosstable) == 2)) { result <- suppressWarnings(chisq.test(crosstable, correct = TRUE))
				} else { result <- suppressWarnings(chisq.test(crosstable, correct = FALSE)) }
				if (any(c(observed, expected, totPercent, rowPercent, colPercent) == TRUE)) {
					# --- PRINTS SOME SUMMAY STATISTICS --- #
					rowPercentage <- prop.table(crosstable, 1) * 100
					colPercentage <- prop.table(crosstable, 2) * 100
					totPercentage <- prop.table(crosstable) * 100
					rowSum <- margin.table(crosstable,1)
					colSum <- margin.table(crosstable,2)
					RowHeading1 <- max(nchar(rowVar[i]), nchar("Total"), nchar(dimnames(crosstable)[[1]])) + 1
					if (length(show.stat) == 1) { RowHeading2 <- 0; addSpace <- 1 } else { RowHeading2 <- max(nobsFreq, nexpFreq, ntotPercent, nrowPercent, ncolPercent) + 1; addSpace <- 2 }
					RowWidth = RowHeading1 + RowHeading2
					ColWidth <- max(nchar(crosstable), max(nchar(round(result[[7]], 0)), nchar(round(rowPercentage, 0)), nchar(round(colPercentage, 0)), nchar(round(totPercentage, 0))) + 5, nchar(dimnames(crosstable)[[2]]), nchar("Total")) + 2
					RowSpace1 <- paste(rep("-", RowWidth+addSpace), collapse = "")
					RowSpace2 <- paste(rep("-", ColWidth+1), collapse = "")
					show.numdigits <- max(nchar(round(rowPercentage,4)), nchar(round(colPercentage,4)), nchar(round(totPercentage,4)))

					options(width = 5000, digits = 6)
					if (length(show.stat) > 1) { cat("Table of ", rowVar[i], " by ", colVar[j], "\n\n", sep = "")
					} else {
						if (observed) cat("Table of Observed Frequency: ", rowVar[i], " by ", colVar[j], "\n\n", sep = "")
						if (expected) cat("Table of Expected Frequency: ", rowVar[i], " by ", colVar[j], "\n\n", sep = "")
						if (totPercent) cat("Table of Total Percentage: ", rowVar[i], " by ", colVar[j], "\n\n", sep = "")
						if (colPercent) cat("Table of Column Percentage: ", rowVar[i], " by ", colVar[j], "\n\n", sep = "")
						if (rowPercent) cat("Table of Row Percentage: ", rowVar[i], " by ", colVar[j], "\n\n", sep = "")
					}
					cat(paste(formatC("", width = RowWidth + addSpace, format = "s", flag = "-"),"|", formatC(colVar[j], width = ColWidth, format = "s", flag = "-"), sep = "", collapse = "\n"), "\n")
					cat(RowSpace1, "+", sep = "")
					for (z in (1:ncol(crosstable))) cat(RowSpace2, "+", sep = "")
					if (RTHeading) cat(RowSpace2, "+", sep = "")
					cat("\n", formatC(rowVar[i], width = RowWidth + addSpace, format = "s", flag = "-"), "|", sep = "")
					for (z in (1:ncol(crosstable))) cat(formatC(dimnames(crosstable)[[2]][z], width = ColWidth+1, format = "s", flag = "-"), "|", sep = "")
					if (observed || expected || totPercent || rowPercent) {
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
							    	if (length(show.stat) > 1) { cat(formatC("Expected Freq", width = RowHeading2, format = "s", flag = "-"),"|", sep = "") }
								for (z in (1:ncol(crosstable))) cat(formatC(result[[7]][k,z], digits = 4, width = ColWidth + 1, format = "f", flag = "#"), "|", sep = "")
								if (RTHeading) cat(formatC(rowSums(result[[7]])[[k]], digits = 4, width = ColWidth + 1, format = "f", flag = "#"), "|", "\n", sep = "")
							} else {
								if (length(show.stat) > 1) {
									cat(formatC("", width = RowHeading1 + 1, format = "s"), "|", sep = "")
									cat(formatC("Expected Freq", width = RowHeading2, format = "s", flag = "-"), "|", sep = "")
								}
								for (z in (1:ncol(crosstable))) cat(formatC(colSums(result$exp)[[z]], digits = 4, width = ColWidth + 1, format = "f", flag = "#"), "|", sep = "")
								if (RTHeading) cat(formatC(sum(result$exp), digits = 4, width = ColWidth + 1, format = "f", flag = "#"), "|","\n", sep = "")
							}
						} ### end -- if (expected)

						if (rowPercent && k <= nrow(crosstable)) {
							if (show.stat[1] != "rowPercent") { cat(formatC("", width = RowHeading1 + 1, format = "s"), "|", sep = "") }
							if (length(show.stat) > 1) { cat(formatC(paste("% within", rowVar[i]), width = RowHeading2, format = "s", flag = "-"), "|", sep = "") }
						    	for (z in (1:ncol(crosstable))) cat(formatC(rowPercentage[k,z], digits = 4, width = ColWidth + 1, format = "f", flag = "#"), "|", sep = "")
							if (RTHeading) cat(formatC(sum(rowPercentage[k,]), digits = 4, width = ColWidth + 1, format = "f", flag = "#"), "|", "\n", sep = "")
						} ### end -- if (rowPercent && k <= nrow(crosstable))

						if (colPercent) {
							if (k <= nrow(crosstable)) {
								if (show.stat[1] != "colPercent") { cat(formatC("", width = RowHeading1 + 1, format = "s"), "|", sep = "") }
								if (length(show.stat) > 1) { cat(formatC(paste("% within", colVar[j]), width = RowHeading2, format = "s", flag = "-"),"|", sep = "") }
							    	for (z in (1:ncol(crosstable))) cat(formatC(colPercentage[k,z], digits = 4, width = ColWidth + 1, format = "f", flag = "#"),"|", sep = "")
								if (RTHeading) cat(formatC("", width = ColWidth + 1, format = "s"),"|", "\n",sep = "") else cat("\n", sep = "")
							} else {
								if (length(show.stat) > 1) {
									cat(formatC("", width = RowHeading1 + 1, format = "s"), "|", sep = "")
									cat(formatC(paste("% within", colVar[j]), width = RowHeading2, format = "s", flag = "-"),"|", sep = "")
								}
							    	for (z in (1:ncol(crosstable))) cat(formatC(colSums(colPercentage)[[z]], digits = 4, width = ColWidth + 1, format = "f", flag = "#"),"|", sep = "")
								if (RTHeading) { cat(formatC("", width = ColWidth + 1, format = "s"),"|", "\n",sep = "") } else { cat("\n", sep = "") }
							}
						} ### end -- if (colPercent)

						if (totPercent) {
							if (k <= nrow(crosstable)) {
							    	if (show.stat[1] != "totPercent") cat(formatC("", width = RowHeading1 + 1, format = "s"), "|", sep = "")
								if (length(show.stat) > 1) { cat(formatC("% of Total", width = RowHeading2, format = "s", flag = "-"), "|", sep = "") }
						    		for (z in (1:ncol(crosstable))) cat(formatC(totPercentage[k,z], digits = 4, width = ColWidth + 1, format = "f", flag = "#"), "|", sep = "")
								if (RTHeading) cat(formatC(rowSums(totPercentage)[[k]], digits = 4, width = ColWidth + 1, format = "f", flag = "#"), "|", "\n",sep = "")
							} else {
								if (length(show.stat) > 1) {
									cat(formatC("", width = RowHeading1 + 1, format = "s"), "|", sep = "")
									cat(formatC("% of Total", width = RowHeading2, format = "s", flag = "-"), "|", sep = "")
								}
						    		for (z in (1:ncol(crosstable))) cat(formatC(colSums(totPercentage)[[z]], digits = 4, width = ColWidth + 1, format = "f", flag = "#"), "|", sep = "")
								if (RTHeading) cat(formatC(sum(totPercentage), digits = 4, width = ColWidth + 1, format = "f", flag = "#"), "|", "\n",sep = "")
							}
						} ### end -- if (totPercent)
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
				} ### if (any(c(observed, expected, totPercent, rowPercent, colPercent) == TRUE))

				if (chisq || phi || cramersv || contingency) {
					chisq.label <- NULL
					if (chisq) {
						chisq.label <- c(chisq.label, "Pearson Chi-Square", "Likelihood Ratio Chi-Square")
						if (all(dim(crosstable) == 2)) { fisher.result <- fisher.test(crosstable, alternative = "two.sided"); chisq.label <- c(chisq.label, "Fisher's Exact Test")	}
						tlable.length2 <- max(nchar(result[[2]][[1]]),2) + 2
						tlable.length3 <- nchar(round(result[[1]][[1]], 4)) + 2
					} else { tlable.length2 <- 0; tlable.length3 <- 0 }
					if (phi) 		{ chisq.label <- c(chisq.label, "Phi Coefficient") }
					if (cramersv) 	{ chisq.label <- c(chisq.label, "Cramer's V") }
					if (contingency) 	{ chisq.label <- c(chisq.label, "Contingency Coefficient") }
					tlable.length1 <- max(nchar(chisq.label)) + 2
					ncell <- nrow(crosstable) * ncol(crosstable)
					NExpFreqless1 <- length(result[[7]][which(result[[7]] < 1)])
					NExpFreqless5 <- length(result[[7]][which(result[[7]] < 10)])
					cat("Statistics for Table ", rowVar[i], " by ", colVar[j], "\n\n", sep = "")

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
    						    formatC(result[[1]][[1]], digits = 4, width = tlable.length3, format = "f", flag = "#"),
						    formatC(result[[3]][[1]], digits = 4, width = 8, format = "f", flag = "#"), "\n", sep = "")
						cat(formatC(chisq.label[2], width = tlable.length1, format = "s", flag = "-"),
						    formatC(likelihood.ratio(crosstable)$df, width = tlable.length2, flag = "-"),
	    					    formatC(likelihood.ratio(crosstable)$statistics, digits = 4, width = tlable.length3, format = "f", flag = "#"),
						    formatC(likelihood.ratio(crosstable)$p.value, digits = 4, width = 8, format = "f", flag = "#"), "\n", sep = "")
					}

					if (phi) {
						cat(formatC("Phi Coefficient", width = tlable.length1, format = "s", flag = "-"), sep = "")
						if (chisq) cat(formatC("", width = tlable.length2, format = "s", flag = "-"), sep = "")
					    	cat(formatC(phi(crosstable), digits = 4, width = tlable.length3, format = "f", flag = "#"), "\n", sep = "")
					}
					if (contingency) {
						cat(formatC("Contingency Coefficient", width = tlable.length1, format = "s", flag = "-"), sep = "")
						if (chisq) cat(formatC("", width = tlable.length2, format = "s", flag = "-"), sep = "")
					    	cat(formatC(contingency(crosstable), digits = 4, width = tlable.length3, format = "f", flag = "#"), "\n", sep = "")
					}
					if (cramersv) {
						cat(formatC("Cramer's V", width = tlable.length1, format = "s", flag = "-"), sep = "")
						if (chisq) cat(formatC("", width = tlable.length2, format = "s", flag = "-"), sep = "")
					    	cat(formatC(cramer(crosstable), digits = 4, width = tlable.length3, format = "f", flag = "#"), "\n", sep = "")
					}
					if (all(dim(crosstable) == 2) && chisq) {
						cat(formatC("Fisher's Exact Test", width = tlable.length1, format = "s", flag = "-"),
						    formatC("", width = tlable.length2, format = "s", flag = "-"),
						    formatC("", width = tlable.length3, format = "s", flag = "-"),
						    formatC(fisher.result[[1]], digits = 4, width = 8, format = "f", flag = "#"), "\n", sep = "")
					}
					cat(formatC(paste(rep("-", tlable.length1), collapse = ""), width = tlable.length1), sep = "")
					if (chisq) cat(formatC(paste(rep("-", tlable.length2), collapse = ""), width = tlable.length2), sep = "")
					cat(formatC(paste(rep("-", tlable.length3), collapse = ""), width = tlable.length3), sep = "")
					if (chisq) cat(formatC(paste(rep("-", 9), collapse = ""), width = 9), sep = "")
					cat("\n")

					num.error <- 0
					if (NExpFreqless5 > 0) { cat(paste(rep("*", num.error <- num.error + 1), collapse = "", sep = ""), " Cells with Expected Frequency < 5: ", NExpFreqless5, " of ", ncell, " (",round((NExpFreqless5/ncell)*100, 2),"%)\n", sep = "") }
					if (NExpFreqless1 > 0) { cat(paste(rep("*", num.error <- num.error + 1), collapse = "", sep = ""), " Cells with Expected Frequency < 1: ", NExpFreqless1, " of ", ncell, " (",round((NExpFreqless1/ncell)*100, 2),"%)\n", sep = "") }
					cat("\n")
				} ### end -- if (chisq || phi || cramersv || contingency)

				if (stdresid) {
					cat("Residuals for Table ", rowVar[i], " by ", colVar[j], "\n\n", sep = "")
					cat(paste(formatC("", width = RowHeading1 + addSpace, format = "s", flag = "-"),"|", formatC(colVar[j], width = ColWidth, format = "s", flag = "-"), sep = "", collapse = "\n"), "\n")
					cat(paste(rep("-",RowHeading1+addSpace), collapse = ""), "+", sep = "")
					for (z in (1:ncol(crosstable))) cat(RowSpace2, "+", sep = "")
					cat("\n", formatC(rowVar[i], width = RowHeading1 + addSpace, format = "s", flag = "-"), "|", sep = "")
					for (z in (1:ncol(crosstable))) cat(formatC(dimnames(crosstable)[[2]][z], width = ColWidth+1, format = "s", flag = "-"), "|", sep = "")
					cat("\n")
					cat(paste(rep("-", RowHeading1+addSpace), collapse = ""), "+", sep = "")
					for (z in (1:ncol(crosstable))) cat(RowSpace2, "+", sep = "")
					cat("\n")
					for (k in (1:nrow(crosstable))) {
						cat(formatC(dimnames(crosstable)[[1]][k], width = RowHeading1 + addSpace, format = "s", flag = "-"), "|",sep = "")
						for (z in (1:ncol(crosstable))) {
							cat(formatC(result[[8]][k,z], digits = 4, width = ColWidth + 1, format = "f", flag = "#"), "|",sep = "")
						}
						cat("\n")
					}
					cat(paste(rep("-", RowHeading1+addSpace), collapse = ""), "+", sep = "")
					for (z in (1:ncol(crosstable))) cat(RowSpace2, "+", sep = "")
					cat("\n\n")
				}
			} ### if (rowVar[i] != colVar[j])
		} ## for (j in (1:length(colVar)))
	} ## for (i in (1:length(rowVar)))
} ### end -- Crosstab function
