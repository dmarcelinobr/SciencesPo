#' @encoding UTF-8
#' @title Cross-tabulation
#' @description \code{crosstable} produces all possible two-way tabulations of the variables specified.
#' @param \dots The data paremeters.
#' @param deparse.level Integer controlling the construction of labels in the case of non-matrix-like arguments. If 0, middle 2 rownames, if 1, 3 rownames, if 2, 4 rownames (default).
#' @return Well-formatted cross tabulation. Also can genarate latex syntax of cross tabulation.
#' @examples
#' with(titanic, crosstable( SEX, AGE))
#' with(titanic, tab( SEX, AGE, SURVIVED))
#'
#' # Agresti (2002), table 3.11, p. 106
#' GSS <- data.frame(
#'    expand.grid(sex=c("female", "male"),
#'    party=c("dem", "indep", "rep")),
#'    count=c(279,165,73,47,225,191))
#'
#' gender = rep(c("female","male"),c(1835,2691))
#' admitted = rep(c("yes","no","yes","no"),c(557,1278,1198,1493))
#' dept = rep(c("A","B","C","D","E","F","A","B","C","D","E","F"),
#'            c(89,17,202,131,94,24,19,8,391,244,299,317))
#' dept2 = rep(c("A","B","C","D","E","F","A","B","C","D","E","F"),
#'            c(512,353,120,138,53,22,313,207,205,279,138,351))
#' department = c(dept,dept2)
#' ucb = data.frame(gender,admitted,department)
#' with(ucb, tab(admitted, gender))
#' @keywords Tables Descriptive
#' @export
#' @rdname tab
crosstable <- function(..., deparse.level = 2){
  table <-  base::table(..., deparse.level = deparse.level)
  class(table) <- c("crosstable", "table")

  summary.crosstable <- function(table, digits=2, latex=FALSE, tests=TRUE, ...){
    x      <- table
    class(x) <- "table"
    sep    <- ifelse(latex, "&", " ")
    .twoDimTable <- function(x, digits=2, width=6){
      output <- NULL
      dim    <- dim(x)
      dimnames <- dimnames(x)
      varnames <- names(dimnames)
      if(latex) varnames[2] <- sprintf("\\multicolumn{%s}{c}{%s}", dim[2], varnames[2])
      x <- stats::addmargins(x, margin=1)
      p <- base::prop.table(x, margin=1) * 100
      x <- stats::addmargins(x, margin=2)
      p <- stats::addmargins(p, margin=2)
      p[is.nan(p)] <- 0


      rowcat <-  base::paste(c(dimnames[[1]], " "), " ", sep="\t", collapse="\t")
      rowcat <-  base::strsplit(rowcat, "\t")[[1]]
      rowcat[length(rowcat)-1] <- "Total"
      rowcat <-  base::format(c(" ", " ", varnames[1], rowcat), justify="left")
      #    rowvar <- c(" ", " ", " ", varnames[1], rep(" ", length(rowcat)-4))
      #    rowvar[length(rowvar)-1] <- "Total"
      #    rowvar <- format(rowvar)

      for(i in seq_len(dim[2])){
        count   <- x[, i]
        percent <-  base::format(p[, i], digits=digits)
        if(latex)
          percent <-  base::paste(percent, "\\%", sep="")
        else
          percent <-  base::paste(percent, "%", sep="")

        col <-  base::paste(count, percent, sep="\t", collapse="\t")
        col <-  base::strsplit(col, "\t")[[1]]
        col <-  base::format(col, justify="right", width=width)
        col <- c(dimnames[[2]][i], col)
        if(latex) col[1] <- sprintf("\\multicolumn{1}{c}{%s}", col[1])
        col <-  base::format(col, justify="centre")
        if(is.null(output))
          output <- col
        else
          output <-  base::paste(output, col, sep=sep)
      }
      i <- dim[2]+1
      count   <- x[, i]
      percent <-  base::format(p[, i], digits=digits)
      if(latex)
        percent <-  base::paste(percent, "\\%", sep="")
      else
        percent <-  base::paste(percent, "%", sep="")

      col <-  base::paste(count, percent, sep="\t", collapse="\t")
      col <-  base::strsplit(col, "\t")[[1]]
      col <-  base::format(col, justify="right", width=width)
      if(latex){
        col <- c(" ", " ", "\\multicolumn{1}{c}{Total}", col)
      } else {
        col <- c(" ", " ", "Total", col)
      }
      col <-  base::format(col, justify="centre")

      nchar  <-  base::nchar(output[1], type="width")
      line1  <-  base::paste(rep("-", nchar), collapse="")
      output <-  base::format(c(varnames[2], line1, output), justify="centre")
      output <-  base::paste(output, col, sep=sep)
      output <-  base::paste(rowcat, output, sep=sep)
      nchar  <-  base::nchar(output[1], type="width")

      #    output <- paste(rowvar, output, sep=sep)
      nchar  <- nchar(output[1], type="width")
      if(latex) {
        output <-  base::paste(output, "\\\\")
        line1 <- "\\midrule"
        line2 <- "\\toprule"
        line3 <- "\\bottomrule"
      } else {
        line1  <-  base::paste(rep("-", nchar), collapse="")
        line2  <-  base::paste(rep("=", nchar), collapse="")
        line3  <-  base::paste(rep("=", nchar), collapse="")
      }

      output <- c(line2, output[1:3], line1, output[4:length(output)], line3)
      output <- c(output[1:(length(output)-3)], line1,
                  output[(length(output)-2):length(output)])
      return(output)
    }

    dim <- dim(x)
    varnames <- names(dimnames(x))
    if(length(dim) == 2) {
      # Two dimensional
      output <- .twoDimTable(x)
      if(latex) output[3] <- sprintf("\\cline{%s-%s}", 2, 2+dim[2]-1)
      output <- base::paste(output, collapse="\n")
      if(latex){

        output <- sprintf("\\begin{table}[htbp]
                          \\centering
                          \\caption{%s $\\times$ %s}
                          \\begin{tabular}{l%s}
                          %s
                          \\end{tabular}
                          \\end{table}",
                          varnames[1], varnames[2],
                          base::paste(rep("r",dim[2]+1), collapse=""), output)
      }


      cat(output, fill=TRUE)
      cat("\n")
      cat("Chi-Square Test for Independence", fill=TRUE)
      if(tests){
        print(summary(vcd::assocstats(x)))
      } else {
        cat("\n")
        print(base::summary.table(x))
      }
    } else {
      # Three Dimensional
      stratumcat <- dimnames(x)[[1]]
      stratumvar <- varnames[1]
      stratumcat <- base::format(c(stratumvar, stratumcat, "Total"), justify="left")
      stratumvar <- stratumcat[ 1]
      stratumcat <- stratumcat[-1]
      output <- list()
      col    <- list()
      width  <-  base::nchar(as.character(max(x)))
      width  <- ifelse(width > 6, width, 6)
      for(i in seq_len(dim[1])) {
        x.tmp <- base::as.table(x[i, , ])
        output[[i]] <- .twoDimTable(x.tmp, width=width)
      }
      total <- base::margin.table(x, c(2, 3))
      output[[dim[1]+1]] <- .twoDimTable(total, width=width)

      output.header <- output[[1]][2:4]
      if(latex) output.header[2] <- sprintf("\\cline{%s-%s}", 3, 3+dim[3]-1)
      output.header[1] <-  base::paste(
        base::paste(rep(" ",  base::nchar(stratumvar)+2), collapse=""),
        output.header[1], sep=sep)
      output.header[2] <- paste(
        base::paste(rep(" ",  base::nchar(stratumvar)+2), collapse=""),
        output.header[2], sep=sep)
      output.header[3] <-  base::paste(stratumvar, output.header[3], sep=sep)

      output <- lapply(output, function(x) return(x[ -c(1:5, length(x))]))
      for(i in seq_along(output)) {
        col         <- c(stratumcat[i], rep(" ", length(output[[i]])-1))
        col         <- base::format(col, justify="left")
        output[[i]] <- base::paste(col, output[[i]], sep=sep)
        nchar  <-  base::nchar(output[[i]][1], type="width")
        if(latex)
          line <- "\\midrule"
        else
          line <-  base::paste(rep("-", nchar), collapse="")

        output[[i]] <- c(output[[i]], line)
      }
      output <- unlist(output)
      output <- output[-length(output)]
      #    col    <- c(stratumvar, rep(" ", length(output)-1))
      #    col    <- format(col)
      #    output <- paste(col, output, sep=sep)

      nchar  <-  base::nchar(output[1], type="width")
      if(latex) {
        line1 <- "\\midrule"
        line2 <- "\\toprule"
        line3 <- "\\bottomrule"
      } else {
        line1  <-  base::paste(rep("-", nchar), collapse="")
        line2  <-  base::paste(rep("=", nchar), collapse="")
        line3  <-  base::paste(rep("=", nchar), collapse="")
      }
      output <- c(line2, output.header, line1, output, line3)
      if(latex){
        output <- gsub("&\\\\cline",   "\\\\cline", output)
        output <- gsub("&\\\\midrule", "\\\\midrule", output)
      }
      output <- paste(output, collapse="\n")
      if(latex){

        output <- sprintf("\\begin{table}[htbp]
                          \\centering
                          \\caption{%s $\\times$ %s $\\times$ %s}
                          \\begin{tabular}{ll%s}
                          %s
                          \\end{tabular}
                          \\end{table}",
                          varnames[1], varnames[2], varnames[3],
                          base::paste(rep("r",dim[3]+1), collapse=""),output)}

      cat(output, fill=TRUE)
      cat("\n")
      cat("Chi-Square Test for Independence", fill=TRUE)
      cat("\n")
      for(i in seq_len(dim[1])) {
        x.tmp <-  base::as.table(x[i, , ])
        cat(sprintf("%s : %s", names(dimnames(x))[1], stratumcat[i]), fill=TRUE)

        if(tests){
          print(summary(vcd::assocstats(x.tmp)))
        } else {
          cat("\n")
          print(base::summary.table(x.tmp))
        }
        cat("\n")
      }
      cat("Total", fill=TRUE)

      if(tests){
        print(summary(vcd::assocstats(base::margin.table(x, c(2, 3)))))
      } else {
        cat("\n")
        print(summary.table(margin.table(x, c(2, 3))))
      }
      cat("\n")
    }
  }
  return(summary.crosstable(table))
    }
NULL


#' @title Cross-tabulation
#' @export
`tab` <- function(...){
  crosstable(..., deparse.level = 2)
}
NULL


