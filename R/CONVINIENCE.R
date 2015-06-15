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
#' @param file the name of the file which the data are to be read from.
#' @param data the internal name after attaching the data file.
#' @param clear if \code{clear = TRUE}, all attached data in the environment will be detached first.
#' @param spss.missing whether SPSS missing values should be replaced with NA; default is \code{spss.missing = TRUE}.
#' @param tolower  whether variable names should be forced to lower case; default is \code{tolower = TRUE}.
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
  function (file,  data = .data, clear = TRUE, spss.missing = TRUE, tolower = TRUE)
  {
    if (clear) {
      detachAll()
    }

    if (is.character(file)) {
      ext <- tolower(substring(file, first = nchar(file) -
                                 3, last = nchar(file)))
      if (ext == ".dta") {
        dataset <- read.dta(file)
      }
      else {
        if (ext == ".sav") {
          data0 <- read.spss(file)
          var.labels <- attr(data0, "variable.labels")
          dataset <- read.spss(file, to.data.frame=TRUE, trim.factor.names=TRUE)
          dataset <- dataset[1:nrow(dataset), 1:ncol(dataset)]
          attr(dataset, "var.labels") <- var.labels
          if(spss.missing){
            for(i in 1:ncol(dataset)){
              if(!is.null(attr(data0, "missing")[[i]]$value)){
                dataset[,i] <- ifelse((dataset[,i] %in% attr(data0, "missing")[[i]]$value),NA,dataset[,i])
              }
              if(!is.null(attributes(data0[[i]])$value.labels)){
                dataset[,i] <- ifelse((dataset[,i] %in% attributes(data0[[i]])$value.labels),NA,dataset[,i])
              }
            }
            if (tolower)
              names(dataset) <- tolower(names(dataset))
          }
          else {
            if (substring(file, first = nchar(file) -
                          3, last = nchar(file)) == ".tsv") {
              dataset <- read.delim(file, header = TRUE,
                                    sep = "\t", stringsAsFactors=FALSE)
            }
            else {
              if (substring(file, first = nchar(file) -
                            3, last = nchar(file)) == ".csv") {
                dataset <- read.csv(file, header = TRUE,
                                    sep = ",", stringsAsFactors=FALSE )
              }
              else {
                stop("This type of file cannot be 'used'.")
              }
            }
          }
        }
      }
    }
    else {
      if (is.data.frame(file)) {
        dataset <- file
      }
      else {
        stop("The argument is not a data frame or no such file")
      }
    }
    nrOfRows <- nrow(dataset);
    nrOfCols <- ncol(dataset);
    assign(as.character(substitute(data)), value=dataset, envir = sys.frame(-1))
    message(paste0('[', nrOfRows, " x ", nrOfCols, ']', " assigned to `.data`", sep=""));
    #attach(dataset, name=as.character(substitute(data)), warn.conflicts = FALSE)
  }
NULL


#' @encoding UTF-8
#' @title View random n elements of an object.
#'
#' @description Provide a sly view of the data by randomly drawn observations, instead of showing only the first \code{head()} or the last \code{tail()} rows of an object, though it is also possible to constrain for the first/last elements.
#'
#' @param obj A matrix or data.frame object
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
`view` <- function (obj=.data, n=10, random=TRUE, tail=FALSE, print.console=TRUE, ...) {
  getn=function(n,N,tail,random,...) {
    n=min(n,N)
    if (random) return(sample(1:N,n,...))
    n1=ifelse(tail,N-n+1,1); n2=ifelse(tail,N,n)
    return(n1:n2) }
  showVec=function(obj,n,tail,random,...){
    N=length(obj); if (N==0) return("empty vector")
    v.vec=obj[getn(n,N,tail,random,...)]
    return(v.vec) }
  showTab=function(obj,n,tail,random,...){
    N=nrow(obj); if (N==0) return("empty table")
    mess = paste(c("v.tab=obj[getn(n,N,tail,random,...)",rep(",",length(dim(obj))-1),"]"),collapse="")
    eval(parse(text=mess))
    return(v.tab) }
  showLis=function(obj,n,tail,random,print.console,...){
    nL=length(obj); if (nL==0) return("empty list")
    v.lis=list()
    if (is.null(names(obj))) ii=1:nL else ii=names(obj)
    for (i in 1:nL) {
      iobj=obj[[i]]
      v.lis = c(v.lis,list(view(iobj,n,tail,random,print.console=FALSE,...)))
    }
    names(v.lis)=ii; return(v.lis) }
  showAll=function(obj){
    return(obj) }
  # End Subfunction------------------------------
  if (n==0) return("nada")
  n=abs(n) # coerce to positive
  if (is.data.frame(obj) || is.matrix(obj) || is.array(obj))
    viewed=showTab(obj,n,tail,random,...)
  else if (is.list(obj))
    viewed=showLis(obj,n,tail,random,print.console,...)
  else if (is.vector(obj) || is.integer(obj) || is.numeric(obj) || is.character(obj))
    viewed=showVec(obj,n,tail,random,...)
  else viewed=showAll(obj)
  if (print.console) print(viewed)
  invisible(viewed)
}
NULL








#' @title Cross-tabulation
#' @description \code{crosstab} produces all possible two-way tabulations of the variables specified.
#' @param \dots the data paremeters.
#' @param row.vars the row variable(s).
#' @param col.vars the row variable(s).
#' @param type wether \emph{frequency count} \code{"f"}, \emph{row percentages} \code{"r"}, \emph{column percentages} \code{"c"}, or \emph{total percentages} \code{"t"}.
#' @param style a style for table, either \code{"wide"} or \code{"long"}.
#' @param decimals an integer for decimal places.
#' @param percent wether to show percentiles or not. Default is \code{TRUE}
#' @param margins wether to add margins or not. Default is \code{TRUE}
#' @param subtotals wether to show subtotals or not. Default is \code{TRUE}
#'
#' @note Adapted from Dr Paul Williamson, Dept. of Geography and Planning, School of Environmental Sciences, University of Liverpool, UK, who adapted from \code{ctab()}.
#' @examples
#' data(titanic)
#'
# Frequency count
#' crosstab(titanic, row.vars = "AGE", col.vars = "SEX", type = "f")
#' # Row percentages
#' crosstab(titanic, row.vars = "AGE", col.vars = "SEX", type = "r")
#'
#' # Column percentages
#' crosstab(titanic, row.vars = "AGE", col.vars = "SEX", type = "c")
#'
#' # Joint percentages (sums to 100 within final two table dimensions)
#' crosstab(titanic, row.vars = c("AGE","SEX"), col.vars =
#' "SURVIVED", type = "c")
#'
#' # Total percentages (sums to 100 across entire table)
#' crosstab(titanic, row.vars = c("AGE","SEX"), col.vars =
#' "SURVIVED", type = "t")
#' # Style = 'long'
#' crosstab(titanic, row.vars = c("AGE","SEX"), col.vars =
#' "SURVIVED", type = "t",style = "long", margins = FALSE)
#'
#' # Style = 'wide' [default]
#' crosstab(titanic, row.vars = "AGE", col.vars =
#' "SURVIVED", type = "t", style = "long", margins = FALSE)
#' @export
#' @examples
#' data(quga)
`crosstab` <- function (..., row.vars = NULL,
                        col.vars = NULL,
                        type = NULL,
                        style = "wide",
                        decimals = 2,
                        percent = TRUE,
                        margins = TRUE,
                        subtotals=TRUE)

  #Declare function used to convert frequency counts into relevant type of proportion or percentage
{
  mk.pcnt.tbl <- function(tbl, type) {
    a <- length(row.vars)
    b <- length(col.vars)
    mrgn <- switch(type, column.pct = c(row.vars[-a], col.vars),
                   row.pct = c(row.vars, col.vars[-b]),
                   joint.pct = c(row.vars[-a], col.vars[-b]),
                   total.pct = NULL)
    tbl <- prop.table(tbl, mrgn)
    if (percent) {
      tbl <- tbl * 100
    }
    tbl
  }

  #Find no. of vars (all; row; col) for use in subsequent code
  n.row.vars <- length(row.vars)
  n.col.vars <- length(col.vars)
  n.vars <- n.row.vars + n.col.vars

  #Check to make sure all user-supplied arguments have valid values
  stopifnot(as.integer(decimals) == decimals, decimals > -1)
  #type: see next section of code
  stopifnot(is.character(style))
  stopifnot(is.logical(percent))
  stopifnot(is.logical(margins))
  stopifnot(is.logical(subtotals))
  stopifnot(n.vars>=1)

  #Convert supplied table type(s) into full text string (e.g. "f" becomes "frequency")
  #If invalid type supplied, failed match gives user automatic error message
  types <- NULL
  choices <- c("frequency", "row.pct", "column.pct", "joint.pct", "total.pct")
  for (tp in type) types <- c(types, match.arg(tp, choices))
  type <- types

  #If no type supplied, default to 'frequency + total' for univariate tables and to
  #'frequency' for multi-dimenstional tables

  #For univariate table....
  if (n.vars == 1) {
    if (is.null(type)) {
      # default = freq count + total.pct
      type <- c("frequency", "total.pct")
      #row.vars <- 1
    } else {
      #and any requests for row / col / joint.pct must be changed into requests for 'total.pct'
      type <- ifelse(type == "frequency", "frequency", "total.pct")
    }
    #For multivariate tables...
  } else if (is.null(type)) {
    # default = frequency count
    type <- "frequency"
  }

  #Check for integrity of requested analysis and adjust values of function arguments as required

  if ((margins==FALSE) & (subtotals==FALSE)) {
    warning("WARNING: Request to suppress subtotals (subtotals=FALSE) ignored because no margins requested (margins=FALSE)")
    subtotals <- TRUE
  }

  if ((n.vars>1) & (length(type)>1) & (margins==TRUE)) {
    warning("WARNING: Only row totals added when more than one table type requested")
    #Code lower down selecting type of margin implements this...
  }

  if ((length(type)>1) & (subtotals==FALSE)) {
    warning("WARNING: Can only request supply one table type if requesting suppression of subtotals; suppression of subtotals not executed")
    subtotals <- TRUE
  }

  if ((length(type)==1) & (subtotals==FALSE)) {
    choices <- c("frequency", "row.pct", "column.pct", "joint.pct", "total.pct")
    tp <- match.arg(type, choices)
    if (tp %in% c("row.pct","column.pct","joint.pct")) {
      warning("WARNING: subtotals can only be suppressed for tables of type 'frequency' or 'total.pct'")
      subtotals<- TRUE
    }
  }

  if ((n.vars > 2) & (n.col.vars>1) & (subtotals==FALSE))
    warning("WARNING: suppression of subtotals assumes only 1 col var; table flattened accordingly")


  if ( (subtotals==FALSE) & (n.vars>2) )  {
    #If subtotals not required AND total table vars > 2
    #Reassign all but last col.var as row vars
    #[because, for simplicity, crosstabs assumes removal of subtotals uses tables with only ONE col var]
    #N.B. Subtotals only present in tables with > 2 cross-classified vars...
    if (length(col.vars)>1) {
      row.vars <- c(row.vars,col.vars[-length(col.vars)])
      col.vars <- col.vars[length(col.vars)]
      n.row.vars <- length(row.vars)
      n.col.vars <- 1
    }
  }

  #If decimals not set by user, set to 2 unlesss only one table of type frequency requested,
  #in which case set to 0.  [Leaves user with possibility of having frequency tables with > 0 dp]
  if (is.null(decimals)) {
    if ((length(type)==1) & (type[1]=="frequency")) {
      decimals <- 0
    } else {
      decimals <-2
    }
  }

  #Take the original input data, whatever form originally supplied in,
  #convert into table format using requested row and col vars, and save as 'tbl'

  args <- list(...)

  if (length(args) > 1) {
    if (!all(sapply(args, is.factor)))
      stop("If more than one argument is passed then all must be factors")
    tbl <- table(...)
  }
  else {
    if (is.factor(...)) {
      tbl <- table(...)
    }
    else if (is.table(...)) {
      tbl <- eval(...)
    }
    else if (is.data.frame(...)) {
      #tbl <- table(...)
      if (is.null(row.vars) && is.null(col.vars)) {
        tbl <- table(...)
      }
      else {
        var.names <- c(row.vars,col.vars)
        A <- (...)
        tbl <- table(A[var.names])
        if(length(var.names==1)) names(dimnames(tbl)) <- var.names
        #[table() only autocompletes dimnames for multivariate crosstabs of dataframes]
      }
    }
    else if (class(...) == "ftable") {
      tbl <- eval(...)
      if (is.null(row.vars) && is.null(col.vars)) {
        row.vars <- names(attr(tbl, "row.vars"))
        col.vars <- names(attr(tbl, "col.vars"))
      }
      tbl <- as.table(tbl)
    }
    else if (class(...) == "ctab") {
      tbl <- eval(...)
      if (is.null(row.vars) && is.null(col.vars)) {
        row.vars <- tbl$row.vars
        col.vars <- tbl$col.vars
      }
      for (opt in c("decimals", "type", "style", "percent",
                    "margins", "subtotals")) if (is.null(get(opt)))
                      assign(opt, eval(parse(text = paste("tbl$", opt,
                                                          sep = ""))))
      tbl <- tbl$table
    }
    else {
      stop("first argument must be either factors or a table object")
    }
  }

  #Convert supplied table style into full text string (e.g. "l" becomes "long")
  style <- match.arg(style, c("long", "wide"))

  #Extract row and col names to be used in creating 'tbl' from supplied input data
  nms <- names(dimnames(tbl))
  z <- length(nms)
  if (!is.null(row.vars) && !is.numeric(row.vars)) {
    row.vars <- order(match(nms, row.vars), na.last = NA)
  }
  if (!is.null(col.vars) && !is.numeric(col.vars)) {
    col.vars <- order(match(nms, col.vars), na.last = NA)
  }
  if (!is.null(row.vars) && is.null(col.vars)) {
    col.vars <- (1:z)[-row.vars]
  }
  if (!is.null(col.vars) && is.null(row.vars)) {
    row.vars <- (1:z)[-col.vars]
  }
  if (is.null(row.vars) && is.null(col.vars)) {
    col.vars <- z
    row.vars <- (1:z)[-col.vars]
  }

  #Take the original input data, converted into table format using supplied row and col vars (tbl)
  #and create a second version (crosstab) which stores results as percent if a percentage table type is requested.
  if (type[1] == "frequency")
    crosstab <- tbl
  else
    crosstab <- mk.pcnt.tbl(tbl, type[1])


  #If multiple table types requested, create and add these to
  if (length(type) > 1) {
    tbldat <- as.data.frame.table(crosstab)
    z <- length(names(tbldat)) + 1
    tbldat[z] <- 1
    pcntlab <- type
    pcntlab[match("frequency", type)] <- "Count"
    pcntlab[match("row.pct", type)] <- "Row %"
    pcntlab[match("column.pct", type)] <- "Column %"
    pcntlab[match("joint.pct", type)] <- "Joint %"
    pcntlab[match("total.pct", type)] <- "Total %"
    for (i in 2:length(type)) {
      if (type[i] == "frequency")
        crosstab <- tbl
      else crosstab <- mk.pcnt.tbl(tbl, type[i])
      crosstab <- as.data.frame.table(crosstab)
      crosstab[z] <- i
      tbldat <- rbind(tbldat, crosstab)
    }
    tbldat[[z]] <- as.factor(tbldat[[z]])
    levels(tbldat[[z]]) <- pcntlab
    crosstab <- xtabs(Freq ~ ., data = tbldat)
    names(dimnames(crosstab))[z - 1] <- ""
  }


  #Add margins if required, adding only those margins appropriate to user request
  if (margins==TRUE) {

    vars <- c(row.vars,col.vars)

    if (length(type)==1) {
      if (type=="row.pct")
      { crosstab <- addmargins(crosstab,margin=c(vars[n.vars]))
      tbl <- addmargins(tbl,margin=c(vars[n.vars]))
      }
      else
      { if (type=="column.pct")
      { crosstab <- addmargins(crosstab,margin=c(vars[n.row.vars]))
      tbl <- addmargins(tbl,margin=c(vars[n.row.vars]))
      }
        else
        { if (type=="joint.pct")
        { crosstab <- addmargins(crosstab,margin=c(vars[(n.row.vars)],vars[n.vars]))
        tbl <- addmargins(tbl,margin=c(vars[(n.row.vars)],vars[n.vars]))
        }
          else #must be total.pct OR frequency
          { crosstab <- addmargins(crosstab)
          tbl <- addmargins(tbl)
          }
        }
      }
    }

    #If more than one table type requested, only adding row totals makes any sense...
    if (length(type)>1) {
      crosstab <- addmargins(crosstab,margin=c(vars[n.vars]))
      tbl <- addmargins(tbl,margin=c(vars[n.vars]))
    }

  }


  #If subtotals not required, and total vars > 2, create dataframe version of table, with relevent
  #subtotal rows / cols dropped [Subtotals only present in tables with > 2 cross-classified vars]
  t1 <- NULL
  if ( (subtotals==FALSE) & (n.vars>2) )  {

    #Create version of crosstab in ftable format
    t1 <- crosstab
    t1 <- ftable(t1,row.vars=row.vars,col.vars=col.vars)

    #Convert to a dataframe
    t1 <- as.data.frame(format(t1),stringsAsFactors=FALSE)

    #Remove backslashes from category names AND colnames
    t1 <- apply(t1[,],2, function(x) gsub("\"","",x))
    #Remove preceding and trailing spaces from category names to enable accurate capture of 'sum' rows/cols
    #[Use of grep might extrac category labels with 'sum' as part of a longer one or two word string...]
    t1 <- apply(t1,2,function(x) gsub("[[:space:]]*$","",gsub("^[[:space:]]*","",x)))

    #Reshape dataframe to that variable and category labels display as required
    #(a) Move col category names down one row; and move col variable name one column to right
    t1[2,(n.row.vars+1):ncol(t1)] <- t1[1,(n.row.vars+1):ncol(t1)]
    t1[1,] <- ""
    t1[1,(n.row.vars+2)] <- t1[2,(n.row.vars+1)]
    #(b) Drop the now redundant column separating the row.var labels from the table data + col.var labels
    t1 <- t1[,-(n.row.vars+1)]

    #In 'lab', assign category labels for each variable to all rows (to allow identification of sub-totals)
    lab <- t1[,1:n.row.vars]
    for (c in 1:n.row.vars) {
      for (r in 2:nrow(lab)) {
        if (lab[r,c]=="") lab[r,c] <- lab[r-1,c]
      }
    }

    lab <- (apply(lab[,1:n.row.vars],2,function(x) x=="Sum"))
    lab <- apply(lab,1,sum)
    #Filter out rows of dataframe containing subtotals

    t1 <- t1[((lab==0) | (lab==n.row.vars)),]

    #Move the 'Sum' label associated with last row to the first column; in the process
    #setting the final row labels associated with other row variables to ""
    t1[nrow(t1),1] <- "Sum"
    t1[nrow(t1),(2:n.row.vars)] <- ""

    #set row and column names to NULL
    rownames(t1) <- NULL
    colnames(t1) <- NULL

  }

  #Create output object 'result' [class: crosstab]
  result <- NULL
  #(a) record of argument values used to produce tabular output
  result$row.vars <- row.vars
  result$col.vars <- col.vars
  result$decimals <- decimals
  result$type <- type
  result$style <- style
  result$percent <- percent
  result$margins <- margins
  result$subtotals <- subtotals

  #(b) tabular output [3 variants]
  result$table <- tbl  #Stores original cross-tab frequency counts without margins [class: table]
  result$crosstab <- crosstab #Stores cross-tab in table format using requested style(frequency/pct) and table margins (on/off)
  #[class: table]
  result$crosstab.nosub <- t1  #crosstab with subtotals suppressed [class: dataframe; or NULL if no subtotals suppressed]
  class(result) <- "crosstab"

  #Return 'result' as output of function
  result

}
NULL


`print.crosstab` <- function(x,decimals=x$decimals,subtotals=x$subtotals,...) {

  row.vars <- x$row.vars
  col.vars <- x$col.vars
  n.row.vars <- length(row.vars)
  n.col.vars <- length(col.vars)
  n.vars <- n.row.vars + n.col.vars

  if (length(x$type)>1) {
    z<-length(names(dimnames(x$crosstab)))
    if (x$style=="long") {
      row.vars<-c(row.vars,z)
    } else {
      col.vars<-c(z,col.vars)
    }
  }

  if (n.vars==1) {
    if (length(x$type)==1) {
      tmp <- data.frame(round(x$crosstab,x$decimals))
      colnames(tmp)[2] <- ifelse(x$type=="frequency","Count","%")
      print(tmp,row.names=FALSE)
    } else {
      print(round(x$crosstab,x$decimals))
    }
  }


  #If table has only 2 dimensions, or subtotals required for >2 dimensional table,
  #print table using ftable() on x$crosstab
  if ((n.vars == 2) | ((subtotals==TRUE) & (n.vars>2))) {

    tbl <- ftable(x$crosstab,row.vars=row.vars,col.vars=col.vars)

    if (!all(as.integer(tbl)==as.numeric(tbl))) tbl <- round(tbl,decimals)
    print(tbl,...)

  }

  #If subtotals NOT required AND > 2 dimensions, print table using write.table() on x$crosstab.nosub
  if ((subtotals==FALSE) & (n.vars>2))  {

    t1 <- x$crosstab.nosub

    #Convert numbers to required decimal places, right aligned
    width <- max( nchar(t1[1,]), nchar(t1[2,]), 7 )
    decimals <- x$decimals
    number.format <- paste("%",width,".",decimals,"f",sep="")
    t1[3:nrow(t1),((n.row.vars+1):ncol(t1))] <- sprintf(number.format,as.numeric(t1[3:nrow(t1),((n.row.vars+1):ncol(t1))]))

    #Adjust column variable label to same width as numbers, left aligned, padding with trailing spaces as required
    col.var.format <- paste("%-",width,"s",sep="")
    t1[1,(n.row.vars+1):ncol(t1)] <- sprintf(col.var.format,t1[1,(n.row.vars+1):ncol(t1)])
    #Adjust column category labels to same width as numbers, right aligned, padding with preceding spaces as required
    col.cat.format <- paste("%",width,"s",sep="")
    t1[2,(n.row.vars+1):ncol(t1)] <- sprintf(col.cat.format,t1[2,(n.row.vars+1):ncol(t1)])

    #Adjust row labels so that each column is of fixed width, using trailing spaces as required
    for (i in 1:n.row.vars) {
      width <- max(nchar(t1[,i])) + 2
      row.lab.format <- paste("%-",width,"s",sep="")
      t1[,i] <- sprintf(row.lab.format,t1[,i])
    }

    write.table(t1,quote=FALSE,col.names=FALSE,row.names=FALSE)

  }

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


#' @examples
#' model<- glm(child~parent, data=galton)
#' sumres(model)
`sumres` <-
  function(x) {
    sr <- summary(residuals(x))
    srm <- mean(residuals(x))
    if (abs(srm) < 1e-10){
      sr <- sr[c(1:3,5:6)]
    }
    sr
  }





