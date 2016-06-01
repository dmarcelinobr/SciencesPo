#' @title Print Crosstable style
#' @description Print Crosstable style
#' @encoding UTF-8
#' @family Crosstables
#' @param object The table object.
#' @param digits number of digits after the decimal point.
#' @param chisq if \code{TRUE}, the results of a chi-square test will be shown below the table.
#' @param fisher if \code{TRUE}, the results of a Fisher Exact test will be shown below the table.
#' @param mcnemar if \code{TRUE}, the results of a McNemar test will be shown below the table.
#' @param alt the alternative hypothesis and must be one of "two.sided",
#'  "greater" or "less". Only used in the 2 by 2 case.
#' @param latex if the output is to be printed in latex format.
#' @param \dots extra parameters.
#'
#' @export
`summary.Crosstable` <-
  function(object,
           digits = 2,
           chisq = FALSE,
           fisher = FALSE,
           mcnemar = FALSE,
           alt="two.sided",
           latex = FALSE,
           ...) {
    x      <- object
    class(x) <- "table"
    sep    <- ifelse(latex, "&", " ")
    twoDimTable <- function(x, digits = 2, width = 6) {
      output <- NULL
      dim    <- dim(x)
      dimnames <- dimnames(x)
      varnames <- names(dimnames)
      if (latex)
        varnames[2] <-
        sprintf("\\multicolumn{%s}{c}{%s}", dim[2], varnames[2])
      x <- addmargins(x, margin = 1)
      p <- prop.table(x, margin = 1) * 100
      x <- addmargins(x, margin = 2)
      p <- addmargins(p, margin = 2)
      p[is.nan(p)] <- 0


      rowcat <-
        paste(c(dimnames[[1]], " "), " ", sep = "\t", collapse = "\t")
      rowcat <- strsplit(rowcat, "\t")[[1]]
      rowcat[length(rowcat) - 1] <- "Total"
      rowcat <-
        format(c(" ", " ", varnames[1], rowcat), justify = "left")

      for (i in seq_len(dim[2])) {
        count   <- x[, i]
        percent <- format(p[, i], digits = digits)
        if (latex)
          percent <- paste(percent, "\\%", sep = "")
        else
          percent <- paste(percent, "%", sep = "")

        col <- paste(count, percent, sep = "\t", collapse = "\t")
        col <- strsplit(col, "\t")[[1]]
        col <- format(col, justify = "right", width = width)
        col <- c(dimnames[[2]][i], col)
        if (latex)
          col[1] <- sprintf("\\multicolumn{1}{c}{%s}", col[1])
        col <- format(col, justify = "centre")
        if (is.null(output))
          output <- col
        else
          output <- paste(output, col, sep = sep)
      }
      i <- dim[2] + 1
      count   <- x[, i]
      percent <- format(p[, i], digits = digits)
      if (latex)
        percent <- paste(percent, "\\%", sep = "")
      else
        percent <- paste(percent, "%", sep = "")

      col <- paste(count, percent, sep = "\t", collapse = "\t")
      col <- strsplit(col, "\t")[[1]]
      col <- format(col, justify = "right", width = width)
      if (latex) {
        col <- c(" ", " ", "\\multicolumn{1}{c}{Total}", col)
      } else {
        col <- c(" ", " ", "Total", col)
      }
      col <- format(col, justify = "centre")

      nchar  <- nchar(output[1], type = "width")
      line1  <- paste(rep("-", nchar), collapse = "")
      output <-
        format(c(varnames[2], line1, output), justify = "centre")
      output <- paste(output, col, sep = sep)
      output <- paste(rowcat, output, sep = sep)
      nchar  <- nchar(output[1], type = "width")

      #    output <- paste(rowvar, output, sep=sep)
      nchar  <- nchar(output[1], type = "width")
      if (latex) {
        output <- paste(output, "\\\\")
        line1 <- "\\midrule"
        line2 <- "\\toprule"
        line3 <- "\\bottomrule"
      } else {
        line1  <- paste(rep("-", nchar), collapse = "")
        line2  <- paste(rep("=", nchar), collapse = "")
        line3  <- paste(rep("=", nchar), collapse = "")
      }

      output <-
        c(line2, output[1:3], line1, output[4:length(output)], line3)
      output <- c(output[1:(length(output) - 3)], line1,
                  output[(length(output) - 2):length(output)])
      return(output)
    }

    dim <- dim(x)
    varnames <- names(dimnames(x))
    if (length(dim) == 2) {

      # Two dimensional
      output <- twoDimTable(x)

      if (latex)
        output[3] <- sprintf("\\cline{%s-%s}", 2, 2 + dim[2] - 1)
      output <- paste(output, collapse = "\n")
      if (latex) {
        output <- sprintf(
          "\\begin{table}[htbp]
          \\centering
          \\caption{%s $\\times$ %s}
          \\begin{tabular}{l%s}
          %s
          \\end{tabular}
          \\end{table}",
          varnames[1],
          varnames[2],
          paste(rep("r", dim[2] + 1), collapse = ""),
          output
        )
      }

      cat("\n")
      cat(output, fill = TRUE)
      cat("\n")

###### Association Tests ########
      # cat("Association Statistics", fill=TRUE)
      if(chisq){
      print(base::summary.table(x))
      }
      if (fisher){
      print(stats::fisher.test(x, alternative=alt))
      }
      if(mcnemar){
        print(stats::mcnemar.test(x))
      }

    } else {
      # Three Dimensional
      stratumcat <- dimnames(x)[[1]]
      stratumvar <- varnames[1]
      stratumcat <-
        format(c(stratumvar, stratumcat, "Total"), justify = "left")
      stratumvar <- stratumcat[1]
      stratumcat <- stratumcat[-1]
      output <- list()
      col    <- list()
      width  <- nchar(as.character(max(x)))
      width  <- ifelse(width > 6, width, 6)
      for (i in seq_len(dim[1])) {
        x.tmp <- as.table(x[i, ,])
        output[[i]] <- twoDimTable(x.tmp, width = width)
      }

      # Two-dimmnesions table
      total <- margin.table(x, c(2, 3))
      output[[dim[1] + 1]] <- twoDimTable(total, width = width)

      output.header <- output[[1]][2:4]
      if (latex)
        output.header[2] <- sprintf("\\cline{%s-%s}", 3, 3 + dim[3] - 1)
      output.header[1] <- paste(paste(rep(" ", nchar(stratumvar) + 2), collapse =
                                        ""),
                                output.header[1], sep = sep)
      output.header[2] <- paste(paste(rep(" ", nchar(stratumvar) + 2), collapse =
                                        ""),
                                output.header[2], sep = sep)
      output.header[3] <- paste(stratumvar, output.header[3], sep = sep)

      output <- lapply(output, function(x)
        return(x[-c(1:5, length(x))]))
      for (i in seq_along(output)) {
        col         <- c(stratumcat[i], rep(" ", length(output[[i]]) - 1))
        col         <- format(col, justify = "left")
        output[[i]] <- paste(col, output[[i]], sep = sep)
        nchar  <- nchar(output[[i]][1], type = "width")
        if (latex)
          line <- "\\midrule"
        else
          line <- paste(rep("-", nchar), collapse = "")

        output[[i]] <- c(output[[i]], line)
      }

      output <- unlist(output)
      output <- output[-length(output)]
      #    col    <- c(stratumvar, rep(" ", length(output)-1))
      #    col    <- format(col)
      #    output <- paste(col, output, sep=sep)

      nchar  <- nchar(output[1], type = "width")
      if (latex) {
        line1 <- "\\midrule"
        line2 <- "\\toprule"
        line3 <- "\\bottomrule"
      } else {
        line1  <- paste(rep("-", nchar), collapse = "")
        line2  <- paste(rep("=", nchar), collapse = "")
        line3  <- paste(rep("=", nchar), collapse = "")
      }
      output <- c(line2, output.header, line1, output, line3)
      if (latex) {
        output <- gsub("&\\\\cline",   "\\\\cline", output)
        output <- gsub("&\\\\midrule", "\\\\midrule", output)
      }
      output <- paste(output, collapse = "\n")
      if (latex) {
        output <- sprintf(
          "\\begin{table}[htbp]
          \\centering
          \\caption{%s $\\times$ %s $\\times$ %s}
          \\begin{tabular}{ll%s}
          %s
          \\end{tabular}
          \\end{table}",
          varnames[1],
          varnames[2],
          varnames[3],
          paste(rep("r", dim[3] + 1), collapse = ""),
          output
        )
      }

  cat("\n")
      cat(output, fill = TRUE)
      cat("\n")
      for (i in seq_len(dim[1])) {
        x.tmp <- as.table(x[i, ,])
        cat(sprintf("%s : %s", names(dimnames(x))[1], stratumcat[i]), fill = TRUE)

###### Association Tests ########
        if(chisq){
          print(base::summary.table(x.tmp))
         }
        if (fisher) {
        print(base::summary.table(x.tmp))
        }
        else{
        cat("\n")
        }
      }

# cat("Total", fill = TRUE)

      # if(chisq){
      #   print(summary(assocstats(margin.table(x, c(2, 3)))))
      # } else if (fisher) {
     # cat("\n")
   #   print(base::summary.table(margin.table(x, c(2, 3))))
      # } else {
      cat("\n")
    }
  }
#}
NULL




#' @encoding UTF-8
#' @title Cross-tabulation
#' @description \code{Crosstable} produces all possible two-way and three-way tabulations of variables.
#'
#' @family Crosstables
#' @param \dots the variables for the cross tabulation.
#' @param digits number of digits after the decimal point.
#' @param chisq if \code{TRUE}, the results of a chi-square test will be shown below the table.
#' @param fisher if \code{TRUE}, the results of a Fisher Exact test will be shown below the table.
#' @param mcnemar if \code{TRUE}, the results of a McNemar test will be shown below the table.
#' @param alt the alternative hypothesis and must be one of "two.sided",
#'  "greater" or "less". Only used in the 2 by 2 case.
#' @param latex if the output is to be printed in latex format.
#' @param deparse.level an integer controlling the construction of labels in the case of non-matrix-like arguments. If 0, middle 2 rownames, if 1, 3 rownames, if 2, 4 rownames (default).
#'
#' @seealso \code{\link[stats]{xtabs}}, \code{\link{Frequency}},
#' \code{\link[base]{table}}, \code{\link[base]{prop.table}}
#'
#' @return A cross-tabulated object.
#' @examples
#' with(titanic, Crosstable( SEX, AGE, SURVIVED))
#'
#' #' # Agresti (2002), table 3.10, p. 106
#' # 1992 General Social Survey--Race and Party affiliation
#' gss <- data.frame(
#'    expand.grid(Race=c("black", "white"),
#'    party=c("dem", "indep", "rep")),
#'    count=c(103,341,15,105,11,405))
#'
#' df <- gss[rep(1:nrow(gss), gss[["count"]]), ]
#'
#' with(df, Crosstable(Race, party))
#'
#' # Agresti (1990, p. 61f; 2002, p. 91) Fisher's Tea Drinker
#'  tea <- data.frame(
#'    expand.grid(poured=c("Yes", "No"),
#'    guess=c("Yes", "No")),
#'    count=c(3,1,1,3))
#'
#' # nicer way of recreating long tables
#' data = Untable(tea, freq="count")
#'
#' with(data, Crosstable(poured, guess, fisher=TRUE, alt="greater")) # fisher=TRUE
#'
#' @keywords Exploratory
#' @export
#' @rdname Crosstable
`Crosstable` <-
  function(...,
           digits = 2,
           chisq = FALSE,
           fisher = FALSE,
           mcnemar = FALSE,
           latex = FALSE,
           alt="two.sided",
           deparse.level = 2)
    UseMethod("Crosstable")



#' @export
#' @rdname Crosstable
`Crosstable.default` <- function(...,
                                 digits = 2,
                                 chisq = FALSE,
                                 fisher = FALSE,
                                 mcnemar = FALSE,
                                 alt="two.sided",
                                 latex = FALSE,
                                 deparse.level = 2) {
  table <- table(..., deparse.level = deparse.level)
  class(table) <- c("SciencesPo", "Crosstable", "table")
  return(
    summary.Crosstable(
      table,
      digits = digits,
      chisq = chisq,
      fisher = fisher,
      mcnemar = mcnemar,
      alt = alt,
      latex = latex
    )
  )
}
NULL




#' Cross Tabulation
#'
#' \code{TwoWay} is a modified version of \code{\link[gmodels]{CrossTable}} (\pkg{gmodels}).
#' \code{TwoWay} will print a summary table with cell counts and column proportions (similar to STATA's
#' \code{tabulate}.
#'
#' @param x,y the variables for the cross tabulation.
#' @param digits number of digits for rounding proportions.
#'
#'
`TwoWay` <- function(x, y, digits = 3){

  dig <- digits
  xName <- deparse(substitute(x))
  yName <- deparse(substitute(y))
  x <- factor(x)
  y <- factor(y)
  tab <- table(x, y)
  dim1 <- dimnames(tab)[[1]]
  dim2 <- dimnames(tab)[[2]]

  # Calcs
  Cprop <- prop.table(tab, 2)
  Tprop <- prop.table(tab)
  Obs <- sum(tab)
  Rsum <- rowSums(tab)
  Csum <- colSums(tab)

  # Formatting Setup
  Ctotal.lab <- "TOTAL"
  Rtotal.lab <- "TOTAL"
  maxC <- max(2 + dig, nchar("TOTAL"), nchar(dim2), nchar(tab), nchar(Rsum), nchar(Csum))
  maxR <- max(nchar(dim1), nchar("TOTAL"))
  Ctotal.lab <- formatC(Ctotal.lab, width = maxR, format = "s")
  Rtotal.lab <- formatC(Rtotal.lab, width = maxC, format = "s")
  Lines <- paste(rep("=", 2 + maxC), collapse = "")
  inner.Lines <- paste(rep("=", 1 + maxR), collapse = "")
  Rspaces <- paste(rep(" ", maxR), collapse = "")
  Cspaces <- paste(rep(" ", maxC), collapse = "")
  outer.Column <- formatC(dim1, width = maxR, format = "s")

printTheTable <- function() {
    xyNames <- abbreviate(c(xName,yName), minlength = 5, dot = T)
    cat(rep(Rspaces, ncol(tab)), "[Y]", xyNames[2], collapse = "\n")
    cat("[X]", xyNames[1], collapse = "\n")
    cat(Rspaces,
        formatC(dim2, width = maxC, format = "s"),
        Rtotal.lab,
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
          formatC(c(tab[i, ], Rsum[i]), width = maxC, format = "d"),
          sep = " | ",
          collapse = "\n"
      )
      cat(Rspaces,
          formatC(c(Cprop[i, ],Rsum[i]/Obs), width = maxC, digits = dig, format = "f"),
          sep = " | ",
          collapse = "\n"
      )
      #       cat(Rspaces,
      #           formatC(c(Tprop[i, ],Rsum[i]/Obs), width = maxC, digits = dig, format = "f"),
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
    cat(Ctotal.lab,
        formatC(c(Csum, Obs), width = maxC, format = "d"),
        sep = " | ",
        collapse = "\n"
    )
    #     cat(" Row%", formatC(c(Csum/Obs, sum(Rsum/Obs)), width = maxC, digits = dig, format = "f"),
    #         #       Cspaces,
    #         sep = " | ",
    #         collapse = "\n"
    #     )
    #     cat(Rspaces,
    #         formatC(c(sum(Cprop[,1]),sum(Cprop[,2])), width = maxC, digits = dig, format = "f"),
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

