#' @encoding UTF-8
#' @title Stata-Like Two-Way Tabulation
#'
#' @description The function produces a cross-tabulation with cell counts and column proportions (similar to STATA's \code{tabulate varname1 varname2, col}).
#'
#' @param x,y The variables for the cross tabulation.
#' @param digits The number of digits for rounding proportions.
#' @param missing.include If TRUE, then remove any unused factor levels.
#' @param \dots Additional arguements (currently ignored).
#' @seealso \code{\link[stats]{xtabs}}, \code{\link{CrossTabs}},
#' \code{\link[base]{table}}, \code{\link[base]{prop.table}}
#' @examples
#' # Agresti (2002), table 3.10, p. 106
#' # 1992 General Social Survey- Race and Party affiliation
#' gss <- data.frame(
#'    expand.grid(Race=c("black", "white"),
#'    party=c("dem", "indep", "rep")),
#'    count=c(103,341,15,105,11,405))
#'
#' df <- gss[rep(1:nrow(gss), gss[["count"]]), ]
#'
#' with(df, CrossTabs(Race, party))
#'
#' # Tea-Tasting Experiment data
#'  tea <- data.frame(
#'    expand.grid(poured=c("Yes", "No"),
#'    guess=c("Yes", "No")),
#'    count=c(3,1,1,3))
#'
#' data = untable(tea, freq="count")
#' with(data, CrossTabs(guess, poured))
#' # with(data, CrossTabs(guess, poured, row=TRUE, column=TRUE, fisher=TRUE))
#'
#' @export
#' @rdname CrossTabs
#' @aliases twoway
`CrossTabs` <- function(x, y, digits = 2, missing.include = FALSE, ...) UseMethod("CrossTabs")

#' @rdname CrossTabs
#' @export
`CrossTabs.default`  <- function(x, y, digits = 2, missing.include = FALSE, ...){

  if (missing(y)) {
    if (is.null(dim(x))) {
      TotalN = length(x)
      if (missing.include)
        x <- factor(x,exclude=c())
      else
        x <- factor(x)
      tab <- t(as.matrix(table(x)))
      vector.x <- TRUE
    }
    else if (length(dim(x) == 2)) {
      if (any(x < 0) || any(is.na(x)))
        stop("all entries of x must be nonnegative and finite")
      if (is.null(rownames(x)))
        rownames(x) <- paste("[", 1:nrow(x), ",]", sep = "")
      if (is.null(colnames(x)))
        colnames(x) <- paste("[,", 1:ncol(x), "]", sep = "")
      tab <- x
    }
    else stop("x must be either a vector or a 2 dimensional matrix, if y is not given")
  }
  else {
    if (length(x) != length(y))
      stop("x and y must have the same length")
    TotalN = length(x)
    xName <- deparse(substitute(x))
    yName <- deparse(substitute(y))
    if (missing.include) {
      x <- factor(x,exclude=c())
      y <- factor(y,exclude=c())
    }
    else {
      x <- factor(x)
      y <- factor(y)
    }
    tab <- table(x, y)
    dim1 <- dimnames(tab)[[1]]
    dim2 <- dimnames(tab)[[2]]
  }

  dig <- digits


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
#' @title Cross-tabulation
#'
#' @description Produces a nested crosstable.
#' @param x The row variable.
#' @param y The column variable.
#' @param z The stratum variable.
#' @param chisq A logical. If \code{TRUE} the results of a chi-square are shown.
#' @param \dots Additional arguements (currently ignored)
#' # @examples
#' # with(titanic, nestedTable(SEX, SURVIVED, CLASS))
#'
`nestedTable` <- function(x, y, z, chisq=FALSE,...) {
  n <- levels(factor(z))
  for (i in 1:length(z)) {
    r <- x[z==n[i]]
    c <- y[z==n[i]]
    # maybe change 'names' by labels
    print(paste("Table of (r)", label(x), " by (c) ->",label(y)))
    CrossTabs(r,c,chisq=FALSE,...)
  }
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
#' @seealso \code{\link{freq}}, \code{\link{CrossTabs}}.
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
`Frequency` <- function(.data, x, verbose=TRUE, ...) UseMethod("Frequency")


#' @rdname Frequency
#' @export
`Frequency.default` <- function(.data, x, verbose=TRUE, ...) {
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

