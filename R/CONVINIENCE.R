#' @encoding UTF-8
#' @title Identify columns with at least one NA value
#'
#' @param x a \code{data.frame}
#'
#' @return A message indicating whether any column in \code{x} has missing data and filling missings with the mean of the valid cases.
#' @author Daniel Marcelino \email{dmarcelino@@live.com}
#' @examples
#'     data(ssex)
#'     ssex_fixed <-fix.missing(ssex)
#' @export
`fix.missing` <- function(x)  {
for (i in 1:ncol(x))  {
if (sum(is.na(x[,i])) > 0 ) {
print(paste("column",i,"has missing data"))
mean.col <- mean(x[,i], na.rm=T)
for (j in 1:nrow(x))  {
if (is.na(x[j,i]) ==T)
		  x[j,i] <- mean.col
		  }
	  }
  }
invisible(x)
 }
NULL


#' @encoding UTF-8
#' @title Random Imputation
#'
#' @description Performs random imputation in a vector that contains missing values.
#'
#' @param x a vector whose missing values (\code{NA}) is to be replaced.
#'
#' @details Indeed a very simple but somewhat limited approach is to impute missing values from observed ones chosen randomly with replacement (MCAR), assuming that \deqn{p(R|Z_{obs}, Z_{mis}) = p(R|\phi)}. Sampling with replacement is important since it continues to favor values with higher incidence (preserving the MCAR empirical distribution). It  may also be combined with apply for matrix imputation drills, but keep in mind that it is experimental (actually, I wrote this for teaching purposes).
#'
#' @examples
#' x <- c(1,2,NA,4,5,NA)
#' rand.imput(x)
#' @export
`rand.imput` <- function(x)  {
  gone <- is.na(x)
  there <- x[!gone]
  x[gone] <- sample(x=there,size=sum(gone),replace=TRUE)
  return(x)
}
NULL

#' @encoding UTF-8
#' @title Create Block-randomized designs
#'
#' @description Generate block-randomized designs based on the number of units \code{n} and block size, where the block size is the number of experimental conditions. The number of Independent Variables and the number of levels in each IV are specified as input. The output is a the block randomized design. This function is intended for planning randomized trails.
#'
#' @param blocksize is the number of control blocks or n per block/group.
#' @param n is the total number of subjects or units.
#' @param seed the random number generation seed.
#'
#' @references
#' Alan S Gerber, Donald P Green (2012). \emph{Field experiments: Design, analysis, and interpretation}. WW Norton.
#'
#' RB Morton, KC Williams (2010). \emph{Experimental political science and the study of causality: From nature to the lab}. Cambridge University Press.
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#'
#' blk <- rand.block(blocksize = 20, n = 80, seed = 51)
#' blk;
#' table(blk$block, blk$condition)
#' # let's do some imaginary analysis
#' set.seed(51);
#' blk$y <- rnorm(n = 80, mean = 20, sd = 5)
#'
#' # Let's look at some descriptives:
#' tapply(blk$y, list(blk$condition, blk$block), mean)
#' tapply(blk$y, list(blk$condition, blk$block), sd)
#'
#' # Do the ANOVA and make some graphs
#' # This formula describes the response `y` by both the treatment factor `condition` and the block control `block`. Note that aov() treats `block` as a random error component of the variance, while lm() treats `block` as a fixed effect.
#'
#' fit.aov <- aov(y ~ factor(condition) + factor(block), data=blk)
#' summary(fit.aov) # display Type I ANOVA table
#' drop1(fit.aov,~.,test="F") # type III SS and F Tests
#'
#' # Since the p-value of 0.254 is much greater than the .05 significance level, we cannot reject the null hypothesis that the mean of `y` for each treatment conditions are all equal.
#'
#' model.tables(fit.aov, "means", se=TRUE) # SE for differences, NOT for means
#' # Calculate the pooled standard error of the means.
#' pooled.se = sqrt(1688.1/4)
#'
#' block <- c(1,2,3,4) # the values of the x axis
#' outcome <- c(19.76, 20.03, 18.44, 18.16) # the results from the means output
#' plot(block, outcome, type = "b", ylab = "outcome", xlab = "blocks of experimental conditions", ylim = c(0, 30) )
#'
#' fit.lm <- lm(y ~ factor(condition) + factor(block), data = blk)
#' anova(fit.aov)
#'
#'@export
`rand.block` = function(blocksize, n, seed=NULL){
  if(!is.null(seed)){
    set.seed(seed)
  }
  # blocking factor
  block = rep(1:ceiling(n/blocksize), each = blocksize)
  a1 = data.frame(id= 1: length(block), block, rand=runif(length(block)))
  a2 = a1[order(a1$block,a1$rand),]
  # matching treatment
  a2$condition = rep(c("Treat", "Control"),times = length(block)/2)
  assign = a2[order(a2$id),]
  class(assign) <- c("SciencesPo", "randomize", "data.frame")
  return(assign)
}
NULL



#' @title Flag duplicated observations
#' @description Marks how many times an observation appears in the dataset.
#' @param obj The data object.
#' @param check.by The formula for checking row-wise for duplicates.
#'
#' @examples
#' df <- data.frame(matrix(c(51,42,43,1,22,51,
#'                  92,28,21,1,22,9),ncol=3, byrow = TRUE))
#' colnames(df) <- c("A","B","C")
#' flag(df, check.by = c("A", "B") )
#' @export
`flag` <- function(obj=.data, check.by=NULL){
  DUPS <- duplicated(obj[, check.by])
  k<-1
  for ( i in 1:nrow(obj)) {
    if(!DUPS[i]) {
      obj$flag[i]<-k
    } else {
      k<-k+1
      obj$flag[i]<-k
    }
  }
  return(obj)
}
NULL



#' @title Filling in missing values
#' \code{fillin} replaces missing values with existing observations from other variable or data frame.
#'
#' @param x the data frame with the variable you would like to fill in.
#' @param y the data frame with the variable you would like to use to fill in \code{x}.
#' @param x.var a character string of the name of the variable in \code{x} you want to fill in.
#' @param y.var an optional character string of variable name in \code{y} that you would like to use to fill in.
#' @param key a character vector of variable names that are shared by \code{x} and \code{y} that can be used to join the data frames.
#'
#' @examples
#' # Create data set with missing values
#' data1 <- data.frame(a = sample(c(1,2), 100, rep=TRUE),
#'                     b = sample(c(3,4), 100, rep=TRUE),
#'                    fNA = sample(c(100, 200, 300, 400, NA), 100, rep=TRUE))
#'
#' # Created full data set
#' data2 <- data.frame(a = c(1,2,1,2),
#'                      b = c(3,3,4,4),
#'                      full = c(100, 200, 300, 400))
#'
#' # Fill in missings from data1 with values from data2
#' Filled <- fillin(data1, data2, x.var = "fNA", y.var = "full", key = c("a", "b"))
#' @importFrom data.table data.table
#' @importFrom data.table  :=
#' @export
`fillin` <- function(x, y, x.var, y.var = NULL, key){
  # Give Var2 the same name as var1 if Var2 is NULL
  if (is.null(y.var)){
    y.var <- x.var
  } else {
    y.var <- y.var
  }
  # Give var a generic name
  names(x)[match(x.var, names(x))] <- "x_x"
  names(y)[match(y.var, names(y))] <- "x_y"
  # Convert data frames to data.table type objects
  tempx <- data.table(x, key = key)
  tempy <- data.table(y, key = key)
  # Merge data.tables
  outdata <- tempy[tempx]
  # Tell the user how many values will be filled in
  SubNA <- outdata[, list(x_x, x_y)]
  SubNA <- subset(SubNA, is.na(x_x) & !is.na(x_y))
  print(paste(nrow(SubNA), "NAs were replaced."))
  # Fill in missing values from data1 with values from data2
  outdata <- outdata[is.na(x_x), x_x := x_y]
  # Convert back to data frame
  ans <- data.frame(outdata)
  # Tell the user what the correlation coefficient is between the variables
  SubNoNA <- subset(ans, !is.na(x_x) & !is.na(x_y))
  HowMany <- nrow(SubNoNA)
  corr <- cor(SubNoNA$x_x, SubNoNA$x_y, use = "complete.obs")
  print(paste("The correlation between", x.var, "and", y.var, "is", round(corr, digits = 3), "based on", HowMany, "shared observations." ))
  # Remove uncombined variable and return main variable's name
  names(ans)[match("x_x", names(ans))] <- x.var
  toKeep <- setdiff(names(ans), "x_y")
  ans <- ans[, toKeep]
  ans
}
NULL




#' @title fill NA by previous cell value (fill forward)
#' @description fillForward will carry values forward from one observation to the next, filling in missing values with the previous value.
#' @param var the column that contains NAs
#' @note This is not intended for imputing missing values; it is regarded as a bad choice for missing-value imputation. The intent is, rather, to fill in \dQuote{holes}, where a value should naturally prevail from one observation to the next.
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @examples
#' view(ssex)
#'
#' fill.forward(ssex$Favor)
#'
#' @export
`fill.forward` <- function(var) {
  navals <- which(is.na(var))
  filledvals <- which(! is.na(var))
  # If there would be no NAs following each other, navals-1 would give the
  # entries we need. In our case, however, we have to find the last column filled for
  # each value of NA. We may do this using the following sapply trick:
  fillup <- sapply(navals, function(x) max(filledvals[filledvals < x]))
  # And finally replace the NAs with our data.
  var[navals] <- var[fillup]
  var
}
NULL







#' @encoding UTF-8
#' @title Make a data.frame Rectangular by fill in missing records
#'
#' @description \code{rfill} produces a complete rectangularization table by adding observations with missing data so that all combinations (interactions) of the specified variables exist.
#'
#' @param x a data frame.
#' @param by a vector of at least 2 variables from the data frame. If missing all variables in the data frame will be used.
#' @param fill the value used to fill in, default is \code{fill = 0}.
#'
#' @return a data object of the same class as \code{x}.
#'
#' @examples
#' data <- data.frame(sex=c("female","male","male"),
#' race = c("black","black","white"), y = c(.5,.4,.1), x = c(32,40,53))
#'
#' retangular.fill(data, by=c(sex,race))
#'
#' retangular.fill(data, by=c(sex,race), fill=0)
#'
#' @export
`retangular.fill` <- function(x, by, fill=NA)
{
  if(missing(by)) by=1:ncol(x)
  nl <- as.list(1:ncol(x))
  names(nl) <- names(x)
  vars <- eval(substitute(by), nl, parent.frame())
  xt <- data.frame(table(x[,vars]))
  x0 <- subset(xt, Freq==0)[,-length(xt)]

  if(nrow(x0)==0){
    x
    warning("Nothing to fill")}
  else{
    z <- as.data.frame(x[1:nrow(x0), -vars, drop=FALSE])
    if(dim(z)[2]==1)
      names(z) <- names(x)[-vars]
    z[,] <- fill
    rbind(x, cbind(x0, z))
  }
}
NULL





#' @encoding UTF-8
#' @title Attach exclusively various file formats
#'
#' @description This works rigorously as the \pkg{epicalc}'s \code{use} function, though limited for the file formats it can read. Fundamentally, it replaces the command attach of R and save an object with extension \code{.data}, which becomes the default dataset. All other \code{data.frames} will be detached, by the time of using the \code{use} function, unless the argument \code{clear = FALSE} is specified.
#'
#' @param file The name of the file which the data are to be read from.
#' @param data The internal name after attaching the data file.
#' @param clear If \code{clear = TRUE}, all attached data in the environment will be detached first.
#' @param spss.missing Whether SPSS missing values should be replaced with NA; default is \code{spss.missing = TRUE}.
#' @param tolower Whether variable names should be forced to lower case; default is \code{tolower = TRUE}.
#'
#' @details By using this \dQuote{attach} version, the data becomes available globally usually positioned in the second place, \code{search()}.
#'
#' @importFrom foreign read.dta
#' @importFrom foreign read.spss
#'
#' @examples
#' use(ssex)
#'
#' @export
`use` <-
  function (file, data = .data, clear = TRUE, spss.missing = TRUE, tolower = TRUE)
  {
    if (clear) {
      detach.all()
    }
    if (is.character(file)) {
      ext <- tolower(substring(file, first = nchar(file) -
                                 3, last = nchar(file)))
      if (ext == ".dta") {
        data1 <- read.dta(file)
      }
      else {
        if (ext == ".dbf") {
          data1 <- read.dbf(file)
          if (tolower)
            names(data1) <- tolower(names(data1))
        }
        else {
          if (ext == ".rec") {
            data1 <- read.epiinfo(file)
            if (tolower)
              names(data1) <- tolower(names(data1))
          }
          else {
            if (ext == ".sav") {
              data0 <- read.spss(file)
              var.labels <- attr(data0, "variable.labels")
              data1 <- read.spss(file, to.data.frame=TRUE, trim.factor.names=TRUE)
              data1 <- data1[1:nrow(data1), 1:ncol(data1)]
              attr(data1, "var.labels") <- var.labels
              if(spss.missing){
                for(i in 1:ncol(data1)){
                  if(!is.null(attr(data0, "missing")[[i]]$value)){
                    data1[,i] <- ifelse((data1[,i] %in% attr(data0, "missing")[[i]]$value),NA,data1[,i])
                  }
                  if(!is.null(attributes(data0[[i]])$value.labels)){
                    data1[,i] <- ifelse((data1[,i] %in% attributes(data0[[i]])$value.labels),NA,data1[,i])
                  }
                }
              }
              if (tolower)
                names(data1) <- tolower(names(data1))
            }
            else {
              if (substring(file, first = nchar(file) -
                            3, last = nchar(file)) == ".csv") {
                data1 <- read.csv(file, header = TRUE,
                                  sep = ",")
              }
              else {
                stop("This type of file cannot be 'use'd.")
              }
            }
          }
        }
      }
    }
    else {
      if (is.data.frame(file)) {
        data1 <- file
      }
      else {
        stop("The argument is not a data frame or no such file")
      }
    }
    nr <- nrow(data1);
    nc <- ncol(data1);
    assign(as.character(substitute(data)), data1, pos = 1)
    attach(data1, name = as.character(substitute(data)),
           warn.conflicts = FALSE)
    message(paste0('[', nr, " x ", nc, ']', " assigned to `.data`", sep=""));
    }
NULL


#' @encoding UTF-8
#' @title View random n elements of an object.
#'
#' @description Provide a sly view of the data by randomly drawn observations, instead of showing only the first \code{head()} or the last \code{tail()} rows of an object, though it is also possible to constrain for the first/last elements.
#'
#' @param data A matrix or data.frame object
#' @param n The number of rows to be shown
#' @param random If TRUE, observartions are sampled.
#' @param tail If TRUE, will show n lines from tail.
#' @param print.console If TRUE, dada will be printed on console.
#' @param \dots Some additional parameters.
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#'
#'
#' @keywords Tables
#' @examples
#' use(titanic)
#' view(titanic)
#'
#' @export
`view` <- function (data=.data, n=10, random=TRUE, tail=FALSE, print.console=TRUE, ...) {
  getn=function(n,N,tail,random,...) {
    n=min(n,N)
    if (random) return(sample(1:N,n,...))
    n1=ifelse(tail,N-n+1,1); n2=ifelse(tail,N,n)
    return(n1:n2) }
  showVec=function(data,n,tail,random,...){
    N=length(data); if (N==0) return("empty vector")
    v.vec=data[getn(n,N,tail,random,...)]
    return(v.vec) }
  showTab=function(data,n,tail,random,...){
    N=nrow(data); if (N==0) return("empty table")
    mess = paste(c("v.tab=data[getn(n,N,tail,random,...)",rep(",",length(dim(data))-1),"]"),collapse="")
    eval(parse(text=mess))
    return(v.tab) }
  showLis=function(data,n,tail,random,print.console,...){
    nL=length(data); if (nL==0) return("empty list")
    v.lis=list()
    if (is.null(names(data))) ii=1:nL else ii=names(data)
    for (i in 1:nL) {
      iobj=data[[i]]
      v.lis = c(v.lis,list(view(iobj,n,tail,random,print.console=FALSE,...)))
    }
    names(v.lis)=ii; return(v.lis) }
  showAll=function(data){
    return(data) }
  # End Subfunction------------------------------
  if (n==0) return("nada")
  n=abs(n) # coerce to positive
  if (is.data.frame(data) || is.matrix(data) || is.array(data))
    viewed=showTab(data,n,tail,random,...)
  else if (is.list(data))
    viewed=showLis(data,n,tail,random,print.console,...)
  else if (is.vector(data) || is.integer(data) || is.numeric(data) || is.character(data))
    viewed=showVec(data,n,tail,random,...)
  else viewed=showAll(data)
  if (print.console) print(viewed)
  invisible(viewed)

  nr <- nrow(data);
  nc <- ncol(data);
  message(paste0("A kind of ", '[', nr, " x ", nc, ']', " data object.", sep=""));
}
NULL


#' @encoding UTF-8
#' @title Cross-tabulation
#' @description \code{crosstab} produces all possible two-way tabulations of the variables specified.
#' @param \dots The data paremeters.
#' @param deparse.level Integer controlling the construction of labels in the case of non-matrix-like arguments. If 0, middle 2 rownames, if 1, 3 rownames, if 2, 4 rownames (default).
#' @return Well-formatted cross tabulation. Also can genarate latex syntax of cross tabulation.
#' @examples
#' with(titanic, crosstable( SEX, AGE))
#' with(titanic, crosstable( SEX, AGE, SURVIVED))
#' @export
#' @rdname crosstab
crosstable <- function(..., deparse.level = 2){
      table <- table(..., deparse.level = deparse.level)
      class(table) <- c("crosstable", "table")

    summary.crosstable <- function(table, digits=2, latex=FALSE, assoc.tests=TRUE, ...){
      x      <- table
      class(x) <- "table"
      sep    <- ifelse(latex, "&", " ")
      twoDimTable <- function(x, digits=2, width=6){
        output <- NULL
        dim    <- dim(x)
        dimnames <- dimnames(x)
        varnames <- names(dimnames)
        if(latex) varnames[2] <- sprintf("\\multicolumn{%s}{c}{%s}", dim[2], varnames[2])
        x <- addmargins(x, margin=1)
        p <- prop.table(x, margin=1) * 100
        x <- addmargins(x, margin=2)
        p <- addmargins(p, margin=2)
        p[is.nan(p)] <- 0


        rowcat <- paste(c(dimnames[[1]], " "), " ", sep="\t", collapse="\t")
        rowcat <- strsplit(rowcat, "\t")[[1]]
        rowcat[length(rowcat)-1] <- "Total"
        rowcat <- format(c(" ", " ", varnames[1], rowcat), justify="left")
        #    rowvar <- c(" ", " ", " ", varnames[1], rep(" ", length(rowcat)-4))
        #    rowvar[length(rowvar)-1] <- "Total"
        #    rowvar <- format(rowvar)

        for(i in seq_len(dim[2])){
          count   <- x[, i]
          percent <- format(p[, i], digits=digits)
          if(latex)
            percent <- paste(percent, "\\%", sep="")
          else
            percent <- paste(percent, "%", sep="")

          col <- paste(count, percent, sep="\t", collapse="\t")
          col <- strsplit(col, "\t")[[1]]
          col <- format(col, justify="right", width=width)
          col <- c(dimnames[[2]][i], col)
          if(latex) col[1] <- sprintf("\\multicolumn{1}{c}{%s}", col[1])
          col <- format(col, justify="centre")
          if(is.null(output))
            output <- col
          else
            output <- paste(output, col, sep=sep)
        }
        i <- dim[2]+1
        count   <- x[, i]
        percent <- format(p[, i], digits=digits)
        if(latex)
          percent <- paste(percent, "\\%", sep="")
        else
          percent <- paste(percent, "%", sep="")

        col <- paste(count, percent, sep="\t", collapse="\t")
        col <- strsplit(col, "\t")[[1]]
        col <- format(col, justify="right", width=width)
        if(latex){
          col <- c(" ", " ", "\\multicolumn{1}{c}{Total}", col)
        } else {
          col <- c(" ", " ", "Total", col)
        }
        col <- format(col, justify="centre")

        nchar  <- nchar(output[1], type="width")
        line1  <- paste(rep("-", nchar), collapse="")
        output <- format(c(varnames[2], line1, output), justify="centre")
        output <- paste(output, col, sep=sep)
        output <- paste(rowcat, output, sep=sep)
        nchar  <- nchar(output[1], type="width")

        #    output <- paste(rowvar, output, sep=sep)
        nchar  <- nchar(output[1], type="width")
        if(latex) {
          output <- paste(output, "\\\\")
          line1 <- "\\midrule"
          line2 <- "\\toprule"
          line3 <- "\\bottomrule"
        } else {
          line1  <- paste(rep("-", nchar), collapse="")
          line2  <- paste(rep("=", nchar), collapse="")
          line3  <- paste(rep("=", nchar), collapse="")
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
        output <- twoDimTable(x)
        if(latex) output[3] <- sprintf("\\cline{%s-%s}", 2, 2+dim[2]-1)
        output <- paste(output, collapse="\n")
        if(latex){

          output <- sprintf("\\begin{table}[htbp]
                            \\centering
                            \\caption{%s $\\times$ %s}
                            \\begin{tabular}{l%s}
                            %s
                            \\end{tabular}
                            \\end{table}",
                            varnames[1], varnames[2],
                            paste(rep("r",dim[2]+1), collapse=""), output)
        }


        cat(output, fill=TRUE)
        cat("\n")
        cat("Chi-Square Test for Independence", fill=TRUE)
        if(assoc.tests){
          print(summary(a.test(x)))
        } else {
          cat("\n")
          print(summary.table(x))
        }


      } else {
        # Three Dimensional
        stratumcat <- dimnames(x)[[1]]
        stratumvar <- varnames[1]
        stratumcat <- format(c(stratumvar, stratumcat, "Total"), justify="left")
        stratumvar <- stratumcat[ 1]
        stratumcat <- stratumcat[-1]
        output <- list()
        col    <- list()
        width  <- nchar(as.character(max(x)))
        width  <- ifelse(width > 6, width, 6)
        for(i in seq_len(dim[1])) {
          x.tmp <- as.table(x[i, , ])
          output[[i]] <- twoDimTable(x.tmp, width=width)
        }
        total <- margin.table(x, c(2, 3))
        output[[dim[1]+1]] <- twoDimTable(total, width=width)

        output.header <- output[[1]][2:4]
        if(latex) output.header[2] <- sprintf("\\cline{%s-%s}", 3, 3+dim[3]-1)
        output.header[1] <- paste(
          paste(rep(" ", nchar(stratumvar)+2), collapse=""),
          output.header[1], sep=sep)
        output.header[2] <- paste(
          paste(rep(" ", nchar(stratumvar)+2), collapse=""),
          output.header[2], sep=sep)
        output.header[3] <- paste(stratumvar, output.header[3], sep=sep)

        output <- lapply(output, function(x) return(x[ -c(1:5, length(x))]))
        for(i in seq_along(output)) {
          col         <- c(stratumcat[i], rep(" ", length(output[[i]])-1))
          col         <- format(col, justify="left")
          output[[i]] <- paste(col, output[[i]], sep=sep)
          nchar  <- nchar(output[[i]][1], type="width")
          if(latex)
            line <- "\\midrule"
          else
            line <- paste(rep("-", nchar), collapse="")

          output[[i]] <- c(output[[i]], line)
        }
        output <- unlist(output)
        output <- output[-length(output)]
        #    col    <- c(stratumvar, rep(" ", length(output)-1))
        #    col    <- format(col)
        #    output <- paste(col, output, sep=sep)

        nchar  <- nchar(output[1], type="width")
        if(latex) {
          line1 <- "\\midrule"
          line2 <- "\\toprule"
          line3 <- "\\bottomrule"
        } else {
          line1  <- paste(rep("-", nchar), collapse="")
          line2  <- paste(rep("=", nchar), collapse="")
          line3  <- paste(rep("=", nchar), collapse="")
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
                            paste(rep("r",dim[3]+1), collapse=""),output)
        }

        cat(output, fill=TRUE)
        cat("\n")
        cat("Chi-Square Test for Independence", fill=TRUE)
        cat("\n")
        for(i in seq_len(dim[1])) {
          x.tmp <- as.table(x[i, , ])
          cat(sprintf("%s : %s", names(dimnames(x))[1], stratumcat[i]), fill=TRUE)

          if(assoc.tests){
            print(summary(a.test(x.tmp)))
          } else {
            cat("\n")
            print(summary.table(x.tmp))
          }
          cat("\n")
        }
        cat("Total", fill=TRUE)

        if(assoc.tests){
          print(summary(a.test(margin.table(x, c(2, 3)))))
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



#' @encoding UTF-8
#' @title Parallel sum
#'
#' @description Provides parallel sum like \code{pmin} and \code{pmax} from the base package. The function \code{sum} simply does not help when the objective is to obtain a vector with parallel sum rather than a scalar value.
#'
#' @param \dots One or more unit objects
#' @param na.rm A logical value \code{TRUE} or \code{FALSE}, the default
#'
#' @return A vector containing the parallel sum.
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#'
#' @keywords Misc
#'
#' @examples
#' psum(us2012$Obama, us2012$Romney)
#' swingy <-psum(us2012$Obama, us2012$Romney-100)
#'
#' @export
`psum` <-
  function(..., na.rm=FALSE) {
    x <- list(...)
    rowSums(matrix(unlist(x), ncol=length(x)), na.rm=na.rm)
  }
NULL



#' @encoding UTF-8
#' @title Trim white spaces
#' @description Simply trims spaces from the start, end, and within of a string
#' @param x is a character vector.
#' @param delim is the delimiter, default is white spaces \code{" "}
#'
# trim(" Daniel   Marcelino   Silva ")
`trim` <- function(x, delim = " ") {
  gsub("^\\s+|\\s+$", "",
       gsub(sprintf("\\s+[%s]\\s+|\\s+[%s]|[%s]\\s+",
                    delim, delim, delim), delim, x))
}
NULL


#' @encoding UTF-8
#' @title Conditionally convert vectors to factors
#'
#' @description A generic function and several instances for creating factors from
#' other sorts of data. The primary use case is for vectors that contain
#' few unique values and might be better considered as factors. When
#' applied to a data frame, this is applied to each column in the data.frame.
#'
#' @param x an object.
#' @param max.levels an integer that determines the number of unique values to be coverted. Default is \code{max.levels = 10}.
#' @param ... additional arguments (currently ignored)
#'
#' @examples
#' #Some data
#' ID = 1:10
#' Age = round(rnorm(10,50,1))
#' diag = c("Depression","Bipolar");
#' Diagnosis = sample(diag, 10, replace=TRUE)
#' data = data.frame(ID, Age, Diagnosis)
#' factorize(data$Diagnosis)
#' str(factorize(data))
#' @export
`factorize` <- function(x,  ...) {
  UseMethod("factorize")
}

#' @rdname factorize
#' @export
`factorize.default` <- function(x, ...) {
  x
}

#' @rdname factorize
#' @export
`factorize.numeric` <- function(x, max.levels = 10L, ...){
  if (length(unique(x)) <=  max.levels) return ( factor(x, levels=sort(unique(x))) )
  x
}

#' @rdname factorize
#' @export
`factorize.character` <- function(x, max.levels = 10L, ...){
  if (length(unique(x)) <=  max.levels) return ( factor(x, levels=sort(unique(x))) )
  x
}

#' @rdname factorize
#' @export
`factorize.data.frame` <- function(x, max.levels=10L, ...) {
  as.data.frame( lapply(x, factorize, max.levels=max.levels) )
}
NULL


#' @encoding UTF-8
#' @title Sumarizes the residuals from a linear model
#'
#' @description Simply sumarizes the residuals from a linear model.
#'#' @param x The fitted model object.
#' @examples
#' model<- glm(child~parent, data=galton)
#' sumres(model)
#' @export
`sumres` <-
  function(x) {
    sr <- summary(residuals(x))
    srm <- mean(residuals(x))
    if (abs(srm) < 1e-10){
      sr <- sr[c(1:3,5:6)]
    }
    sr
  }
NULL




#' @encoding UTF-8
#' @title Aggregate a numeric variable
#'
#' @param formula The variable versus factor(s) to be computed (y~factor+factor).
#' @param data The data object.
#' @param FUN The function statistic to be calculated.
#' @examples
#' # data:
#' df=data.frame(group=sample(letters,100, TRUE),y=sample(100) )
#'
#' #functions:
#' FUNS <- function(x) c(N=nobs(x), mean=round(mean(x),0),
#' sd=round(sd(x), 0), min=round(min(x),0),
#' max=round(max(x),0))
#'
#' # Do the computation
#' compute(y~group, data=df, FUN=FUNS)
#' @export
compute <- function(formula, data=.data, FUN){
  if(class(FUN)=="list"){
    f <- function(x) sapply(FUN, function(fun) fun(x))
  }else{f <- FUN}
  temp <- aggregate(formula, data, f)
  out <- data.frame(temp[,-ncol(temp)], temp[,ncol(temp)])
  colnames(out)[1] <- colnames(temp)[1]
  return(out)
}
NULL





#' @encoding UTF-8
#' @title Sort data set and related vector
#'
#' @param \dots Index variable(s) used for sorting.
#' @param data The data frame where all variables of the same length are sorted.
#' @param inclusive Whether vectors outside the default data frame should also be sorted.
#'
#' @examples
#' df=data.frame(group=sample(letters,10, TRUE),y=sample(10) )
#' use(df)
#' y2 <- y^2
#' bysort(y, inclusive = FALSE)
#' y2 # unsorted
#' bysort(y, inclusive = TRUE)
#' y2 # sorted
#' bysort(y, decreasing=TRUE)
#' .data
#' @export
bysort <- function(..., data = .data, inclusive=TRUE) {
  data1 <- data
  data1 <- data1[order(...),]
  if(inclusive){
    y <- setdiff(lsNoFunction(), as.character(ls.str(mode="list")[]))
    if (length(y)>0){
      for(i in 1:length(y)){
        if(length(get(y[i]))==nrow(data1)){
          nam <- y[i]
          assign (nam, (get(y[i]))[order(...)], envir = .GlobalEnv)
        }
      }
    }
  }
  detach.all()
  assign(as.character(substitute(data)), data1, pos=1)
  attach(data1, name=as.character(substitute(data)), warn.conflicts = FALSE)
}
NULL






