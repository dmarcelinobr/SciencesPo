#' @title Cross-tabulation
#'
#' @description \code{crosstable} produces all possible two/three-way tabulations.
#' @param x The row parameter.
#' @param y The column parameter.
#' @param digits Number of digits after the decimal point.
#' @param max.width In the case of a 1 x n table, the default will be to print the output horizontally. If the number of columns exceeds max.width, the table will be wrapped for each successive increment of max.width columns. If you want a single column vertical table, set max.width to 1.
#' @param expected If TRUE, chisq will be set to TRUE and expected cell counts from the Chi-Square will be included.
#' @param row If TRUE, row proportions will be included.
#' @param column If TRUE, column proportions will be included.
#' @param total If TRUE, table proportions will be included.
#' @param resid If TRUE, residual (Pearson) will be included.
#' @param sresid If TRUE, standardized residual will be included.
#' @param asresid If TRUE, adjusted standardized residual will be included.
#' @param chisq If TRUE, the results of a chi-square test will be included.
#' @param fisher If TRUE, the results of a Fisher Exact test will be included.
#' @param mcnemar If TRUE, the results of a McNemar test will be included.
#' @param missing.include If TRUE, then remove any unused factor levels.
#'
#' @author Adaptation of Gregory R. Warnes' CrossTable() function in the gregmisc package.
#' @export
crosstabs <- function (x, y, digits = 2,
                       max.width = 5,
                       expected = FALSE,
                      row = FALSE,
                      column = FALSE,
                      total = FALSE,
                      resid = FALSE,
                      sresid = FALSE,
                      asresid = FALSE,
                      chisq = FALSE,
                      fisher = FALSE,
                      mcnemar = FALSE,
                      missing.include = FALSE)
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
    CPR <- prop.table(t, 1)*1
    CPC <- prop.table(t, 2)*1
    CPT <- prop.table(t)*1
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

    print.crosstabs.default <- function() {
        if (exists("RowData")) {
            cat(SpaceSep1, "|", ColData, "\n")

            cat(cat(formatC(RowData, width = RWidth, format = "s"),sep=" | ",collapse=""),
                cat(formatC(dimnames(t)[[2]], width = CWidth-1, format = "s"),sep="  | ",collapse=""),
                cat(RowTotal, sep = " | ", collapse = "\n"), sep="",collapse="")
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
                    cat(formatC(c(CPR[i, ], 1*RS[i]/GT),
                        width = CWidth-1, digits = digits, format = "f"),
                        sep = "  | ", collapse = "\n"),sep="",collapse = "")
            if (column)
                cat(cat(SpaceSep1,sep=" | ",collapse = ""),
                    cat(formatC(CPC[i, ], width = CWidth-1,
                        digits = digits, format = "f"),sep = "  | ",collapse=""),
                    cat(SpaceSep2,sep = " | ", collapse = "\n"),sep="",collapse="")
            if (total)
                cat(cat(SpaceSep1,sep=" | ",collapse = ""),
                    cat(formatC(CPT[i, ], width = CWidth-1,
                        digits = digits, format = "f"),sep = "  | ", collapse=""),
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
                cat(formatC(1*CS/GT, width = CWidth-1, digits = digits,
                    format = "f"),sep = "  | ", collapse = ""),
                cat(SpaceSep2,sep = " | ", collapse = "\n"),sep="",collapes="")
        cat(RowSep1, rep(RowSep, ncol(t) + 1), sep = "|", collapse = "\n")
    }
    print.crosstabs.vector <- function() {
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
                    digits = digits, format = "f"), sep = "  | ",
                    collapse = ""),sep="",collapse="\n")
            cat(SpaceSep3, rep(RowSep, (end[i] - start[i]) +
                1), sep = "|", collapse = "\n")
        }
    }
    cat("\n")
    cat("   Cell Contents\n")
    cat("|-------------------|\n")
    cat("|             Count |\n")
    if (!vector.x) {
       if (expected)
          cat("|   Expected Values |\n")
       if (row)
          cat("|    Row Proportion |\n")
       if (column)
          cat("| Column Proportion |\n")
       if (total)
          cat("|  Total Proportion |\n")
       if (resid)
          cat("|          Residual |\n")
       if (sresid)
          cat("|      Std Residual |\n")
       if (asresid)
          cat("|     Adj Std Resid |\n")
    }
    else
       cat("|      Row Proportion |\n")
    cat("|-------------------|\n")
    cat("\n")
    cat("Total Observations in Table: ", GT, "\n")
    cat("\n")
    if (!vector.x)
        print.crosstabs.default()
    else print.crosstabs.vector()
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
}
