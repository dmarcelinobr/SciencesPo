#' @encoding UTF-8
#' @title Cross-tabulation
#'
#' @description Makes a contingency table and computes tests of independence.
#' @param x The row variable.
#' @param y The column variable.
#' @param expected If TRUE, expected values are included.
#' @param row If TRUE, row proportions are included.
#' @param column If TRUE, column proportions are included.
#' @param total If TRUE, total proportions are included.
#' @param resid If TRUE, residual (Pearson) are included.
#' @param sresid If TRUE, standardized residuals are included.
#' @param asresid If TRUE, adjusted standardized residual are included.
#' @param chisq If TRUE, the results of chi-square test of independence are included.
#' @param fisher If TRUE, the results of a Fisher Exact are included.
#' @param mcnemar If TRUE, the results of a McNemar test are included.
#' @param missing.include If TRUE, then remove any unused factor levels.
#' @param digits The number of digits after the decimal point for cell proportions.
#' @param max.width In the case of a 1 x n table, the default will be to print the output horizontally. If the number of columns exceeds max.width, the table will be wrapped for each successive increment of max.width columns. If you want a single column vertical table, set max.width to 1.
#' @param \dots Additional arguements (currently ignored)
#'
#' @note
#' Heavily adapted from Gregory R. Warnes' CrossTable() function in the gregmisc package.
#' @examples
#' # titanic %>% CrossTabs(SEX, AGE)
#'
#' # Tea-Tasting Experiment data
#'  tea <- data.frame(
#'    expand.grid(poured=c("Yes", "No"),
#'    guess=c("Yes", "No")),
#'    count=c(3,1,1,3))
#'
#' data = untable(tea, freq="count")
#'
#' with(data, CrossTabs(guess, poured, row=TRUE, column=TRUE, fisher=TRUE))
#'
#' # Agresti (2002), table 3.10, p. 104
#' # 1991 General Social Survey: hypothesis of independence between
#' # party identification and race.
#' gss <- data.frame(
#'    expand.grid(race=c("black", "white"),
#'    party=c("dem", "indep", "rep")),
#'    count=c(103,341,15,105,11,405))
#'
#' data = untable(gss, freq="count")
#'
#' with(data, CrossTabs(race, party, chisq=TRUE))
#'
#' @export
#' @rdname CrossTabs
#' @aliases twoway
`CrossTabs` <- function (x, y, expected = TRUE,
                        row = FALSE, column = FALSE, total = FALSE,
                        resid = FALSE, sresid = FALSE, asresid = FALSE,
                        chisq = FALSE, fisher = FALSE, mcnemar = FALSE,
                        missing.include = FALSE, digits = 2, max.width = 5, ...) UseMethod("CrossTabs")

#' @rdname CrossTabs
#' @export
`CrossTabs.default` <- function (x, y, expected = TRUE,
                       row = FALSE, column = FALSE, total = FALSE,
                       resid = FALSE, sresid = FALSE, asresid = FALSE,
                       chisq = FALSE, fisher = FALSE, mcnemar = FALSE,
                       missing.include = FALSE, digits = 3, max.width = 5, ...)
{
  if (max.width < 1)
    stop("max.width must be >= 1")
  vector.x <- FALSE
  if (missing(y)) {
    if (is.null(dim(x))) {
      TotalN = length(x)
      if (missing.include)
        x <- factor(x,exclude=c())
      else
        x <- factor(x)
      t <- t(as.matrix(table(x)))
      vector.x <- TRUE
    }
    else if (length(dim(x) == 2)) {
      if (any(x < 0) || any(is.na(x)))
        stop("all entries of x must be nonnegative and finite")
      if (is.null(rownames(x)))
        rownames(x) <- paste("[", 1:nrow(x), ",]", sep = "")
      if (is.null(colnames(x)))
        colnames(x) <- paste("[,", 1:ncol(x), "]", sep = "")
      t <- x
    }
    else stop("x must be either a vector or a 2 dimensional matrix, if y is not given")
  }
  else {
    if (length(x) != length(y))
      stop("x and y must have the same length")
    TotalN = length(x)
    RowData <- deparse(substitute(x))
    ColData <- deparse(substitute(y))
    if (missing.include) {
      x <- factor(x,exclude=c())
      y <- factor(y,exclude=c())
    }
    else {
      x <- factor(x)
      y <- factor(y)
    }
    t <- table(x, y)
  }
  if (any(dim(t) < 2)) {
    column <- row <- chisq <- expected <- fisher <- mcnemar <- FALSE
  }
  CPR <- prop.table(t, 1)*100
  CPC <- prop.table(t, 2)*100
  CPT <- prop.table(t)*100
  GT <- sum(t)
  if (length(dim(x) == 2))
    TotalN = GT
  RS <- rowSums(t)
  CS <- colSums(t)
  ColTotal <- "Column Total"
  RowTotal <- "Row Total"
  CWidth <- max(digits + 2, c(nchar(t), nchar(dimnames(t)[[2]]),
                              nchar(RS), nchar(CS), nchar(RowTotal)))
  RWidth <- max(c(nchar(dimnames(t)[[1]]), nchar(ColTotal)))
  if (exists("RowData"))
    RWidth <- max(RWidth, nchar(RowData))
  RowSep <- paste(rep("-", CWidth + 2), collapse = "")
  RowSep1 <- paste(rep("-", RWidth + 1), collapse = "")
  SpaceSep1 <- paste(rep(" ", RWidth), collapse = "")
  SpaceSep2 <- paste(rep(" ", CWidth), collapse = "")
  FirstCol <- formatC(dimnames(t)[[1]], width = RWidth, format = "s")
  ColTotal <- formatC(ColTotal, width = RWidth, format = "s")
  RowTotal <- formatC(RowTotal, width = CWidth, format = "s")
  if (chisq) {
    if (all(dim(t) == 2))
      CSTc <- chisq.test(t, correct = TRUE)
    CST <- chisq.test(t, correct = FALSE)
  }
  else
    CST <- suppressWarnings(chisq.test(t, correct = FALSE))
  if (asresid & !vector.x)
    ASR <- (CST$observed-CST$expected)/sqrt(CST$expected*((1-RS/GT) %*% t(1-CS/GT)))



  print.CrossTable.default <- function() {
    if (exists("RowData")) {
      cat(SpaceSep1, "|", ColData, "\n")
      cat(cat(formatC(RowData, width = RWidth, format = "s"),sep=" | ",collapse=""),
          cat(formatC(dimnames(t)[[2]], width = CWidth-1, format = "s"),sep="  | ",collapse=""),
          cat(RowTotal, sep = " | ", collapse = "\n"),sep="",collapse="")
    }
    else cat(SpaceSep1, formatC(dimnames(t)[[2]], width = CWidth,
                                format = "s"), RowTotal, sep = " | ", collapse = "\n")
    cat(RowSep1, rep(RowSep, ncol(t) + 1), sep = "|", collapse = "\n")
    for (i in 1:nrow(t)) {
      cat(cat(FirstCol[i],sep=" | ",collapse=""),
          cat(formatC(c(t[i, ], RS[i]), width = CWidth-1),
              sep = "  | ", collapse = "\n"),sep="",collapse="")
      if (expected)
        cat(cat(SpaceSep1,sep=" | ",collapse=""),
            cat(formatC(CST$expected[i, ], digits = digits,
                        format = "f", width = CWidth-1), sep = "  | ",
                collapse = ""),
            cat(SpaceSep2,sep = " | ", collapse = "\n"),sep="",collapse="")
      if (row)
        cat(cat(SpaceSep1,sep=" | ",collapse = ""),
            cat(formatC(c(CPR[i, ], 100*RS[i]/GT),
                        width = CWidth-1, digits = digits, format = "f"),
                sep = "% | ", collapse = "\n"),sep="",collapse = "")
      if (column)
        cat(cat(SpaceSep1,sep=" | ",collapse = ""),
            cat(formatC(CPC[i, ], width = CWidth-1,
                        digits = digits, format = "f"),sep = "% | ",collapse=""),
            cat(SpaceSep2,sep = " | ", collapse = "\n"),sep="",collapse="")
      if (total)
        cat(cat(SpaceSep1,sep=" | ",collapse = ""),
            cat(formatC(CPT[i, ], width = CWidth-1,
                        digits = digits, format = "f"),sep = "% | ", collapse=""),
            cat(SpaceSep2,sep = " | ", collapse = "\n"),sep="",collapse="")
      if (resid)
        cat(cat(SpaceSep1,sep=" | ",collapse = ""),
            cat(formatC(CST$observed[i, ]-CST$expected[i, ], digits = digits,
                        format = "f", width = CWidth-1), sep = "  | ",
                collapse = ""),
            cat(SpaceSep2,sep = " | ", collapse = "\n"),sep="",collapse="")
      if (sresid)
        cat(cat(SpaceSep1,sep=" | ",collapse = ""),
            cat(formatC(CST$residual[i, ], digits = digits,
                        format = "f", width = CWidth-1), sep = "  | ",
                collapse = ""),
            cat(SpaceSep2,sep = " | ", collapse = "\n"),sep="",collapse="")
      if (asresid)
        cat(cat(SpaceSep1,sep=" | ",collapse = ""),
            cat(formatC(ASR[i, ], digits = digits,
                        format = "f", width = CWidth-1), sep = "  | ",
                collapse = ""),
            cat(SpaceSep2,sep = " | ", collapse = "\n"),sep="",collapse="")
      cat(RowSep1, rep(RowSep, ncol(t) + 1), sep = "|",
          collapse = "\n")
    }
    cat(cat(ColTotal,sep=" | ",collapse=""),
        cat(formatC(c(CS, GT), width = CWidth-1), sep = "  | ",
            collapse = "\n"),sep="",collapse="")
    if (column)
      cat(cat(SpaceSep1,sep=" | ",collapse=""),
          cat(formatC(100*CS/GT, width = CWidth-1, digits = digits,
                      format = "f"),sep = "% | ", collapse = ""),
          cat(SpaceSep2,sep = " | ", collapse = "\n"),sep="",collapes="")
    cat(RowSep1, rep(RowSep, ncol(t) + 1), sep = "|", collapse = "\n")
  }


  print.CrossTable.vector <- function() {
    if (length(t) > max.width) {
      final.row <- length(t)%%max.width
      max <- length(t) - final.row
      start <- seq(1, max, max.width)
      end <- start + (max.width - 1)
      if (final.row > 0) {
        start <- c(start, end[length(end)] + 1)
        end <- c(end, end[length(end)] + final.row)
      }
    }
    else {
      start <- 1
      end <- length(t)
    }
    SpaceSep3 <- paste(SpaceSep2, " ", sep = "")
    for (i in 1:length(start)) {
      cat(cat(SpaceSep2,sep=" | ",collapse=""),
          cat(formatC(dimnames(t)[[2]][start[i]:end[i]],
                      width = CWidth-1, format = "s"), sep = "  | ", collapse = "\n"),
          sep="",collapse="")
      cat(SpaceSep3, rep(RowSep, (end[i] - start[i]) +
                           1), sep = "|", collapse = "\n")
      cat(cat(SpaceSep2,sep=" | ",collapse=""),
          cat(formatC(t[, start[i]:end[i]], width = CWidth-1),
              sep = "  | ", collapse = "\n"),
          sep="",collapse="")
      cat(cat(SpaceSep2, sep=" | ",collapse=""),
          cat(formatC(CPT[, start[i]:end[i]], width = CWidth-1,
                      digits = digits, format = "f"), sep = "% | ",
              collapse = ""),sep="",collapse="\n")
      cat(SpaceSep3, rep(RowSep, (end[i] - start[i]) +
                           1), sep = "|", collapse = "\n")
    }
  }
  cat("\n")
  cat("   Cell Contents\n")
  cat("|-----------------|\n")
  cat("|           Count |\n")
  if (!vector.x) {
    if (expected)
      cat("| Expected Values |\n")
    if (row)
      cat("|     Row Percent |\n")
    if (column)
      cat("|  Column Percent |\n")
    if (total)
      cat("|   Total Percent |\n")
    if (resid)
      cat("|        Residual |\n")
    if (sresid)
      cat("|    Std Residual |\n")
    if (asresid)
      cat("|   Adj Std Resid |\n")
  }
  else
    cat("|     Row Percent |\n")
  cat("|-----------------|\n")
  cat("\n")
  cat("Total Observations in Table: ", GT, "\n")
  cat("\n")
  if (!vector.x)
    print.CrossTable.default()
  else print.CrossTable.vector()
  if (GT < TotalN)
    cat("\nNumber of Missing Observations: ",TotalN-GT," (",100*(TotalN-GT)/TotalN,"%)\n",sep="")

  if (chisq) {
    cat("\n")
    cat("============================================================\n\n")
    cat("Statistics for All Table Factors\n\n")
    cat(CST$method, "\n")
    cat("------------------------------------------------------------\n")
    cat("Chi^2 = ", CST$statistic, "    d.f. = ", CST$parameter,
        "    p = ", CST$p.value, "\n\n")
    if (all(dim(t) == 2)) {
      cat(CSTc$method, "\n")
      cat("------------------------------------------------------------\n")
      cat("Chi^2 = ", CSTc$statistic, "    d.f. = ", CSTc$parameter,
          "    p = ", CSTc$p.value, "\n")
    }
  }
  if (mcnemar) {
    McN <- mcnemar.test(t, correct = FALSE)
    cat("\n")
    cat(McN$method, "\n")
    cat("------------------------------------------------------------\n")
    cat("Chi^2 = ", McN$statistic, "    d.f. = ", McN$parameter,
        "    p = ", McN$p.value, "\n\n")
    if (all(dim(t) == 2)) {
      McNc <- mcnemar.test(t, correct = TRUE)
      cat(McNc$method, "\n")
      cat("------------------------------------------------------------\n")
      cat("Chi^2 = ", McNc$statistic, "    d.f. = ", McNc$parameter,
          "    p = ", McNc$p.value, "\n")
    }
  }
  if (fisher) {
    cat("\n")
    FTt <- fisher.test(t, alternative = "two.sided")
    if (all(dim(t) == 2)) {
      FTl <- fisher.test(t, alternative = "less")
      FTg <- fisher.test(t, alternative = "greater")
    }
    cat("Fisher's Exact Test for Count Data\n")
    cat("------------------------------------------------------------\n")
    if (all(dim(t) == 2)) {
      cat("Sample estimate odds ratio: ", FTt$estimate,
          "\n\n")
      cat("Alternative hypothesis: true odds ratio is not equal to 1\n")
      cat("p = ", FTt$p.value, "\n")
      cat("95% confidence interval: ", FTt$conf.int, "\n\n")
      cat("Alternative hypothesis: true odds ratio is less than 1\n")
      cat("p = ", FTl$p.value, "\n")
      cat("95% confidence interval: ", FTl$conf.int, "\n\n")
      cat("Alternative hypothesis: true odds ratio is greater than 1\n")
      cat("p = ", FTg$p.value, "\n")
      cat("95% confidence interval: ", FTg$conf.int, "\n\n")
    }
    else {
      cat("Alternative hypothesis: two.sided\n")
      cat("p = ", FTt$p.value, "\n")
    }
  }
  cat("\n")
  CT <- list(t = t, prop.row = CPR, prop.col = CPC, prop.tbl = CPT)
  if (any(chisq, fisher, mcnemar)) {
    if (all(dim(t) == 2)) {
      if (chisq)
        CT <- c(CT, list(chisq = CST, chisq.corr = CSTc))
      if (fisher)
        CT <- c(CT, list(fisher.ts = FTt, fisher.tl = FTl,
                         fisher.gt = FTg))
      if (mcnemar)
        CT <- c(CT, list(mcnemar = McN, mcnemar.corr = McNc))
    }
    else {
      if (chisq)
        CT <- c(CT, list(chisq = CST))
      if (fisher)
        CT <- c(CT, list(fisher.ts = FTt))
      if (mcnemar)
        CT <- c(CT, list(mcnemar = McN))
    }
  }
  invisible(CT)

  if (any(dim(t) >= 2) & any(chisq,mcnemar,fisher)) {
    MinExpF = min(CST$expected)
    cat('       Minimum expected frequency:',MinExpF,"\n")
    NMinExpF = length(CST$expected[which(CST$expected<5)])
    if (NMinExpF > 0) {
      NCells = length(CST$expected)
      cat('Cells with Expected Frequency < 5: ',NMinExpF,' of ',NCells," (",100*NMinExpF/NCells,"%)\n",sep="")
    }
    cat("\n")
  }
}##-end crosstabs



NestedCrossTabs <- function(A,B,C,chisq=F,...) {
  n <- levels(factor(C))
  for (i in 1:length(C)) {
    r <- A[C==n[i]]
    c <- B[C==n[i]]
    print(paste("Table of (r)",label(A), " by (c) ->",label(B)))
    CrossTabs(r,c,chisq=F,...)
  }
}
NULL







#' @encoding UTF-8
#' @title Stata-Like Two-Way Tabulation
#'
#' @description The function is a modified version of \code{\link{CrossTabs}} for printing a summary table with cell counts and column proportions (similar to STATA's
#' \code{tabulate varname1 varname2, col}).
#'
#' @param x,y The variables for the cross tabulation.
#' @param digits The number of digits for rounding proportions.
#' @seealso \code{\link[stats]{xtabs}}, \code{\link{CrossTabs}},
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






#' @encoding UTF-8
#' @title Frequency Table
#'
#' @description Simulating the FREQ procedure of SPSS.
#'
#' @param .data The data.frame.
#' @param x A column for which a frequency of values is desired.
#' @param verbose A logical value, if \code{TRUE}, extra statistics are also provided.
#' @param \dots Additional arguements (currently ignored)
#'
#' @seealso \code{\link{freqw}}, \code{\link{CrossTabs}}.
#'
#' @examples
#' data(cathedrals)
#'
#' freq(cathedrals, Type)
#'
#' cathedrals %>% freq(Height)
#'
#' @importFrom stats sd
#' @rdname freq
#' @export
#' @aliases oneway
`freq` <- function(.data, x, verbose=TRUE, ...) UseMethod("Freq")


#' @rdname freq
#' @export
`freq.default` <- function(.data, x, verbose=TRUE, ...) {
  vec <-eval(substitute(x), .data, parent.frame())
  nmiss=sum(is.na(vec))
  fsum=summary(factor(vec))
  ftab=cbind(fsum,100*fsum/sum(fsum))
  if (nmiss==0) {
    ftab=cbind(ftab,100*cumsum(fsum)/sum(fsum))
    colnames(ftab)=c("Frequency"," Valid Percent"," Cum Percent")
    ftab[,2] <- round(ftab[,2],2)
    ftab[,3] <- round(ftab[,3],2)
    if(verbose==FALSE){
      return(ftab)
    }
    print(ftab)
  }
  else
  {
    ftab=cbind(ftab,100*fsum/sum(fsum[1:(length(fsum)-1)]),100*cumsum(fsum)/sum(fsum[1:(length(fsum)-1)]))
    ftab[length(fsum),3:4]=NA
    ftab[,2] <- round(ftab[,2],2)
    ftab[,3] <- round(ftab[,3],2)
    ftab[,4] <- round(ftab[,4],2)
    cat("\n")
    cat("--------------------------------------------------------\n")
    colnames(ftab)=c("Frequency","   Percent"," Valid Percent"," Cum Percent")
    if (dim(ftab)[1]==length(levels(vec)))
    {
      rownames(ftab)[1:length(levels(factor(vec)))]=levels(factor(vec))
    }
    if(verbose==FALSE){
      return(ftab)
    }
    print(ftab)
  }
  cat("--------------------------------------------------------\n")
  cat("Total",rep(" ",8-trunc(log10(sum(fsum)))),sum(fsum),"\n",sep="")
  cat("\n")

  if (length(attributes(vec)$class) != 0)
  {
    if ("factor" %in% attributes(vec)$class)
    {
      cat("Warning: Statistics may not be meaningful for factors!\n\n")
    }
  }
  s1=cbind(mean(as.numeric(vec),na.rm=TRUE),stats::sd(as.numeric(vec),na.rm=TRUE))
  rownames(s1)=" "
  colnames(s1)=c("       Mean","        Std dev")
  print(s1)
  s2=cbind(min(as.numeric(vec),na.rm=TRUE),max(as.numeric(vec),na.rm=TRUE))
  rownames(s2)=" "
  colnames(s2)=c("    Minimum","        Maximum")
  print(s2)
  s3=cbind(sum(!is.na(vec)),nmiss)
  rownames(s3)=" "
  colnames(s3)=c("Valid cases",  "Missing cases")
  print(s3)
  cat("\n")
}#--end of Freq
NULL
