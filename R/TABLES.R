#' @title Simple Frequency Table
#'
#' @description Creates a simple frequency data.frame.
#'
#' @param x A vector of values for which the frequency is desired.
#' @param weighs A vector of weights.
#' @param breaks one of: 1) a vector giving the breakpoints between histogram
#'  cells; 2) a function to compute the vector of breakpoints; 3) a single
#'  number giving the number of cells for the histogram; 4) a character string
#'   naming an algorithm to compute the number of cells (see 'Details'); 5) a
#'    function to compute the number of cells.
#' @param digits The number of significant digits required.
#' @param include.lowest Logical; if \code{TRUE}, an x[i] equal to the breaks value will be included in the first (or last) category or bin.
#' @param order The order method.
#' @param useNA Logical; if \code{TRUE} NA's values are included.
#' @param \dots Additional arguements (currently ignored)
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}.
#'
#' @seealso \code{\link{Frequency}}, \code{\link{crosstable}}.
#'
#' @examples
#' data(Presidents)
#'
#' freq(Presidents$winner.party)
#'
#'
#' @rdname freq
#' @export
`freq` <-
  function(x,
           weighs = NULL,
           breaks = graphics::hist(x, plot = FALSE)$breaks,
           digits = 3,
           include.lowest = TRUE,
           order = c("desc", "asc", "level", "name"),
           useNA = c("no", "ifany", "always"),
           ...)
    UseMethod("freq")

#' @rdname freq
#' @export
`freq.default` <-
  function(x,
           weighs = NULL,
           breaks = graphics::hist(x, plot = FALSE)$breaks,
           digits = 3,
           include.lowest = TRUE,
           order = c("desc", "asc", "level", "name"),
           useNA = c("no", "ifany", "always"),
           ...) {
    # check if x is a vector (do not use is.vector())
    if (!(is.atomic(x) || is.list(x)))
      stop("'x' must be a vector")

    if (inherits(x, "table")) {
      TABLE <- x

    } else {
      if (is.numeric(x)) {
        x <-
          base::cut(
            x,
            breaks = breaks,
            include.lowest = include.lowest,
            ordered_result = TRUE,
            ...
          )
      }

      TABLE <- table(x, useNA = useNA)
    }

    # how should the table be sorted, by name, level or frq? (NULL means "desc")
    switch(
      .Match(
        arg = order,
        choices = c("desc", "asc", "level", "name")
      ),
      level  = {

      }
      ,
      name   = {
        TABLE <- TABLE[rownames(TABLE)]
      }
      ,
      asc    = {
        TABLE <- sort(TABLE)
      }
      ,
      desc   = {
        TABLE <- -sort(-TABLE)
      }
    )

    ptab <- base::prop.table(TABLE)
    names(TABLE)[is.na(names(TABLE))] <- "<NA>"
    out <- data.frame(
      class = names(TABLE),
      Freq = as.vector(TABLE[]),
      Prop = round(as.vector(ptab[]), digits)
    )
    #cumfreq = cumsum(TABLE[]), cumperc = round(cumsum(ptab[]),dig))
    rownames(out) <- NULL # enumerate from 1:nrow(z)
    class(out) <- c("SciencesPo", "freq", "data.frame")
    return(out)
  }##-end of freq
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
#' @seealso \code{\link{freq}}, \code{\link{crosstable}}.
#'
#' @examples
#' data(cathedrals)
#'
#' Frequency(cathedrals, Type)
#'
#' cathedrals %>% Frequency(Height)
#'
#' @importFrom stats sd
#' @rdname Frequency
#' @aliases Freq
#' @export
`Frequency` <-
  function(.data, x, verbose = TRUE, ...)
    UseMethod("Frequency")


#' @rdname Frequency
#' @export
`Frequency.default` <- function(.data, x, verbose = TRUE, ...) {
  vec <- eval(substitute(x), .data, parent.frame())
  nmiss = sum(is.na(vec))
  fsum = summary(factor(vec))
  ftab = cbind(fsum, 100 * fsum / sum(fsum))
  if (nmiss == 0) {
    ftab = cbind(ftab, 100 * cumsum(fsum) / sum(fsum))
    colnames(ftab) = c("Frequency", " Valid Percent", " Cum Percent")
    ftab[, 2] <- round(ftab[, 2], 2)
    ftab[, 3] <- round(ftab[, 3], 2)
    if (verbose == FALSE) {
      return(ftab)
    }
    print(ftab)
  }
  else
  {
    ftab = cbind(ftab, 100 * fsum / sum(fsum[1:(length(fsum) - 1)]), 100 * cumsum(fsum) /
                   sum(fsum[1:(length(fsum) - 1)]))
    ftab[length(fsum), 3:4] = NA
    ftab[, 2] <- round(ftab[, 2], 2)
    ftab[, 3] <- round(ftab[, 3], 2)
    ftab[, 4] <- round(ftab[, 4], 2)
    cat("\n")
    cat("--------------------------------------------------------\n")
    colnames(ftab) = c("Frequency", "   Percent", " Valid Percent", " Cum Percent")
    if (dim(ftab)[1] == length(levels(vec)))
    {
      rownames(ftab)[1:length(levels(factor(vec)))] = levels(factor(vec))
    }
    if (verbose == FALSE) {
      return(ftab)
    }
    print(ftab)
  }
  cat("--------------------------------------------------------\n")
  cat("Total", rep(" ", 8 - trunc(log10(sum(
    fsum
  )))), sum(fsum), "\n", sep = "")
  cat("\n")

  if (length(attributes(vec)$class) != 0)
  {
    if ("factor" %in% attributes(vec)$class)
    {
      cat("Warning: Statistics may not be meaningful for factors!\n\n")
    }
  }
  s1 = cbind(mean(as.numeric(vec), na.rm = TRUE),
             stats::sd(as.numeric(vec), na.rm = TRUE))
  rownames(s1) = " "
  colnames(s1) = c("       Mean", "        Std dev")
  print(s1)
  s2 = cbind(min(as.numeric(vec), na.rm = TRUE), max(as.numeric(vec), na.rm =
                                                       TRUE))
  rownames(s2) = " "
  colnames(s2) = c("    Minimum", "        Maximum")
  print(s2)
  s3 = cbind(sum(!is.na(vec)), nmiss)
  rownames(s3) = " "
  colnames(s3) = c("Valid cases",  "Missing cases")
  print(s3)
  cat("\n")
}#--end of Freq
NULL

#' @export
#' @rdname Frequency
Freq <- Frequency







#' @encoding UTF-8
#' @title Cross-tabulation
#' @description \code{crosstable} produces all possible two-way and three-way tabulations of variables.
#' @param .data The data object.
#' @param \dots The variables for the cross tabulation.
#' @param row \code{TRUE}.
#' @param column \code{TRUE}.
#' @param deparse.level Integer controlling the construction of labels in the case of non-matrix-like arguments. If 0, middle 2 rownames, if 1, 3 rownames, if 2, 4 rownames (default).
#'
#' @seealso \code{\link[stats]{xtabs}}, \code{\link{Frequency}},
#' \code{\link[base]{table}}, \code{\link[base]{prop.table}}
#'
#' @return A cross-tabulated object.
#' @examples
#' titanic %>% crosstable( SEX, AGE, SURVIVED)
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
#' crosstable(df, Race, party)
#'
#' # Tea-Tasting Experiment data
#'  tea <- data.frame(
#'    expand.grid(poured=c("Yes", "No"),
#'    guess=c("Yes", "No")),
#'    count=c(3,1,1,3))
#'
#' # nicer way of recreating long tables
#' data = untable(tea, freq="count")
#'
#' crosstable(data, poured, guess, row=TRUE, column=TRUE) # fisher=TRUE
#'


#' @keywords Exploratory
#' @export
#' @rdname crosstable
`crosstable` <-
  function(.data,
           ...,
           row = TRUE,
           column = TRUE,
           digits=2,
           deparse.level = 2)
    UseMethod("crosstable")

#' @export
#' @rdname crosstable
`crosstable.default` <-
  function(.data,
           ...,
           row = TRUE,
           column = TRUE,
           digits=2,
           deparse.level = 2) {
    #################################################################
    #                                                               #
    # Function created by Daniel Marcelino                          #
    # Dept. of Political Sciences, University of Montreal, Canada   #
    #                                                               #
    # Version: 12th July 2012                                       #
    #                                                               #
    # Best viewed using the companion function print.crosstable()   #
    #                                                               #
    #################################################################
    dig <- digits
    ### dplyr version of table, to improve speed
    ## count for each variable combination present
    res <- regroup(.data, ...)
    ## expand to include all possible variable combinations
    ## (in future may not be necessary, see
    ## https://github.com/hadley/dplyr/issues/341)
    lev <- lapply(res[, -ncol(res)], function(x)
      sort(unique(x)))
    expanded_res <- suppressMessages(merge(expand.grid(lev), res)$n)
    expanded_res[is.na(expanded_res)] <-
      0 # set absent combinations to 0
    ## restructure as table
    table <-
      structure(expanded_res,
                .Dim = unname(lengths(lev)),
                .Dimnames = lev)
    class(table) <- c("SciencesPo", "crosstable", "table")


    print.crosstable <-
      function(table,
               latex = FALSE) {
        tab      <- table
        class(tab) <- "table"
        sep    <- c("&"[latex], " "[!latex])

      .twoDimTable <- function(tab,
                                 digits = dig,
                                 width = 6) {
          output <- NULL
          dim    <- dim(tab)
          dimnames <- dimnames(tab)
          varnames <- names(dimnames)
          if (latex)
            varnames[2] <-
            sprintf("\\multicolumn{%s}{c}{%s}", dim[2], varnames[2])
          if (row == TRUE) {
            # place sum in the last row
            tab <- rbind(tab, Sum = colSums(tab))
            # place sum in the last column
            tab <- cbind(tab, Sum = rowSums(tab))
            p <- tab / tab[, "Sum"]
          }
          else if (column == TRUE) {
            # place sum in the last column
            tab <- cbind(tab, Sum = rowSums(tab))
            # place sum in the last row
            tab <- rbind(tab, Sum = colSums(tab))
            p <- sweep(tab, 2, tab["Sum", ], "/")
          } else {
            # place sum in the last row
            tab <- rbind(tab, Sum = colSums(tab))
            # place sum in the last column
            tab <- cbind(tab, Sum = rowSums(tab))
            p <- sweep(tab, 2, tab["Sum", ], "/")
          }
          names(dimnames(tab)) <- varnames
          class(tab) <- "table"
          p[is.nan(p)] <- 0

          rowcat <- c(rbind(c(dimnames[[1]], "Total"), " "))
          rowcat <-
            format(c(" ", " ", varnames[1], rowcat), justify = "left")

          for (i in seq_len(dim[2])) {
            count   <- tab[, i]
            percent <-  format(p[, i], digits = digits)

            col <- c(rbind(count, percent))
            col <-  format(col, justify = "right", width = width)
            col <- c(dimnames[[2]][i], col)
            if (latex)
              col[1] <- sprintf("\\multicolumn{1}{c}{%s}", col[1])
            col <-  format(col, justify = "centre")
            if (is.null(output))
              output <- col
            else
              output <-  paste(output, col, sep = sep)
          }
          i <- dim[2] + 1
          count   <- tab[, i]
          percent <-  format(p[, i], digits = dig)

          col <-  c(rbind(count, percent))
          col <-  format(col, justify = "right", width = width)

          if (latex) {
            col <- c(" ", " ", "\\multicolumn{1}{c}{Total}", col)
          } else {
            col <- c(" ", " ", "Total", col)
          }
          col <-  format(col, justify = "centre")

          nchar  <-  nchar(output[1], type = "width")
          line1  <-  paste(rep.int("-", nchar), collapse = "")
          output <-
            format(c(varnames[2], line1, output), justify = "centre")
          output <-  paste(output, col, sep = sep)
          output <-  paste(rowcat, output, sep = sep)
          nchar  <-  nchar(output[1], type = "width")

          # output <- paste(rowvar, output, sep=sep)
          nchar  <- nchar(output[1], type = "width")

          if (latex) {
            output <-  paste(output, "\\\\")
            line1 <- "\\midrule"
            line2 <- "\\toprule"
            line3 <- "\\bottomrule"
          } else {
            line1  <-  paste(rep.int("-", nchar), collapse = "")
            line2  <-  paste(rep.int("=", nchar), collapse = "")
            line3  <-  line2
          }

          output <-
            c(line2, output[1:3], line1, output[4:length(output)], line3)
          output <- c(output[1:(length(output) - 3)], line1,
                      output[(length(output) - 2):length(output)])
          return(output)
        }

        dim <- dim(tab)
        varnames <- names(dimnames(tab))
        if (length(dim) == 2) {
          # Two dimensional
          output <- .twoDimTable(tab)

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
              paste(rep.int("r", dim[2] + 1), collapse = ""),
              output
            )
          }
          cat("\n")
          cat(output, fill = TRUE)
          cat("\n") # 2x2 Tests of Independence

        } else {
          # Three Dimensional
          stratumcat <- dimnames(tab)[[1]]
          stratumvar <- varnames[1]
          stratumcat <-
            format(c(stratumvar, stratumcat, "Total"), justify = "left")
          stratumvar <- stratumcat[1]
          stratumcat <- stratumcat[-1]
          output <- list()
          col    <- list()
          width  <-  nchar(as.character(max(tab)))
          width[width <= 6] <-  6
          for (i in seq_len(dim[1])) {
            x.tmp <- as.table(tab[i, ,])
            output[[i]] <- .twoDimTable(x.tmp, width = width)
          }

          total <- margin.table(tab, c(2, 3))
          output[[dim[1] + 1]] <- .twoDimTable(total, width = width)

          output.header <- output[[1]][2:4]
          if (latex)
            output.header[2] <- sprintf("\\cline{%s-%s}", 3, 3 + dim[3] - 1)
          output.header[1] <-  paste(paste(rep.int(" ",  nchar(stratumvar) +
                                                     2), collapse = ""),
                                     output.header[1], sep = sep)
          output.header[2] <- paste(paste(rep.int(" ",  nchar(stratumvar) +
                                                    2), collapse = ""),
                                    output.header[2], sep = sep)
          output.header[3] <-
            paste(stratumvar, output.header[3], sep = sep)


          output <-
            lapply(output, function(x)
              return(x[-c(1:5, length(x))]))
          for (i in seq_along(output)) {
            col         <- c(stratumcat[i], rep.int(" ", length(output[[i]]) - 1))
            col         <- format(col, justify = "left")
            output[[i]] <- paste(col, output[[i]], sep = sep)
            nchar  <-  nchar(output[[i]][1], type = "width")
            if (latex)
              line <- "\\midrule"
            else
              line <-   paste(rep.int("-", nchar), collapse = "")

            output[[i]] <- c(output[[i]], line)
          }
          output <- unlist(output)
          output <- output[-length(output)]
          #    col    <- c(stratumvar, rep(" ", length(output)-1))
          #    col    <- format(col)
          #    output <- paste(col, output, sep=sep)

          nchar  <-  nchar(output[1], type = "width")
          if (latex) {
            line1 <- "\\midrule"
            line2 <- "\\toprule"
            line3 <- "\\bottomrule"
          } else {
            line1  <-  paste(rep.int("-", nchar), collapse = "")
            line2  <-  paste(rep.int("=", nchar), collapse = "")
            line3  <-  line2
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
              paste(rep.int("r", dim[3] + 1), collapse = ""),
              output
            )
          }
          cat("\n");
          cat(output, fill = TRUE);
          cat("\n")
        }
      }
    return(print.crosstable(table))
  }
NULL
