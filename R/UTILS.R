#' @title Chain operator
#' @description Chain operator.
#' @name %>%
#' @export %>%
#' @keywords  internal
#' @rdname chain
#' @usage x %>% f(y) is translated into f(x, y).
`%>%` <- magrittr::`%>%`


#' @encoding UTF-8
#' @title Odds Ratio Calculation
#' @description The orcalc can be used to obtain odds ratios. Simply provide the two probabilities to be used (the probability of success for group 1 is given first, then the probability of success for group 2)
#' @param p1 is a probability for group 1.
#' @param p2 is another probability, for group 2.
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @export
#' @examples
#' orcalc(.3, .4)
#'
`orcalc` <- function(p1, p2){
or = (p2 / (1 - p2)) / (p1 / (1 - p1))
return(round(or,3))
}
NULL



#' @encoding UTF-8
#' @title Trim white spaces
#' @description Simply trims spaces from the start, end, and within of a string
#' @param x is a character vector.
#' @param delim is the delimiter, default is white spaces \code{" "}
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @export
# trim(" Daniel   Marcelino   Silva ")
`trim` <- function(x, delim = " ") {
  gsub("^\\s+|\\s+$", "",
       gsub(
         sprintf("\\s+[%s]\\s+|\\s+[%s]|[%s]\\s+",
                 delim, delim, delim),
         delim,
         x
       ))
}### end -- trim function
NULL




#' @title To Make Anonymous a Data Frame
#'
#' @description Replaces factor and character variables by a
#' combination of letters and numbers. Numeric columns are also transformed, see details.
#' @param .data A vector or a data frame.
#' @param col.names A string for column names.
#' @param row.names A string for rown names, default is empty.
#' @return An object of the same type as \code{.data}.
#'
#' @details Strings will is replaced by an algorithm with 52/102 chance of
#' choosing a letter and 50/102 chance of choosing a number; then it joins
#' everything in a 5-digits long character string.
#'
#' @examples
#' dt <- data.frame(
#' Z = sample(LETTERS,10),
#' X = sample(1:10),
#' Y = sample(c("yes", "no"), 10, replace = TRUE)
#' )
#' dt;
#'
#' anonymize(dt)
#'
#' @export
#' @rdname anonymize
`anonymize` <- function(.data, col.names = "V", row.names = "")
  UseMethod("anonymize")

#' @rdname anonymize
#' @export
`anonymize.default` <- function(.data, col.names = "V", row.names = "") {
  .tweak <- function(x) {
    if(is.factor(x)) {
      levels(x) <- sample(LETTERS, length(levels(x)), replace=TRUE)
    }
    else if(is.numeric(x)) {
      x <- x/mean(x, na.rm=T)
    } else {
      x <- replicate(length(x),
                     paste(sample(c(rep(0:9,each=5),LETTERS,letters), 5,
                                  replace=TRUE), collapse=""))
    }
    return(x)
  }
  ## replace the variable names
  colnames(.data) <- paste(col.names, seq_len(ncol(.data)), sep = "")
  ## fudge any factor levels
  df <- data.frame(lapply(.data,  .tweak))
  ## replace rownames
  rownames(df) <- paste(row.names, seq_len(nrow(df)), sep = "")
  return(df)
}
NULL







#' @encoding UTF-8
#' @title Insert Line Breaks in Long Strings
#' @name textWrap
#'
#' @description Insert line breaks in long character strings. Useful for wrapping labels / titles for plots and tables.
#'
#' @param labels Label(s) as character string, where a line break should be
#' inserted. Several strings may be passed as vector  (see 'Examples').
#' @param wrap Maximum amount of chars per line (i.e. line length). If code{wrap = Inf},
#'  no word wrap will be performed (i.e. \code{labels} will be returned as is).
#' @param linesep By default, this argument is \code{NULL} and a regular new line
#'          string (\code{"\\n"}) is used. For HTML-purposes, for instance, \code{linesep}
#'          could be \code{"<br>"}.
#' @return New label(s) with line breaks inserted at every \code{wrap}'s position.
#'
#' @keywords internal
#' @examples
#' textWrap(c("A very long string", "And another even longer string!"), 10)
#'
#' @export
textWrap <- function(labels, wrap, linesep = NULL) {
  # check for valid value
  if (is.null(labels) || length(labels) == 0)
    return(NULL)
  # infinite wrap? then return labels
  if (is.infinite(wrap))
    return(labels)
  # default line separator is \n
  if (is.null(linesep)) {
    linesep <- '\\1\n'
    lsub <- 0
    ori.linesep <- '\n'
  } else {
    # however, for html-function we can use "<br>"
    # as argument
    lsub <- nchar(linesep) - 1
    ori.linesep <- linesep
    linesep <- sprintf("\\1%s", linesep)
  }
  # create regex pattern for line break
  pattern <- c(paste('(.{1,', wrap, '})(\\s|$)', sep = ""))
  # iterate all labels
  for (n in 1:length(labels)) {
    # check if wrap exceeds lengths of labels
    if (wrap > 0 && nchar(labels[n]) > wrap) {
      # insert line breaks
      labels[n] <- gsub(pattern, linesep, labels[n])
      # -----------------------
      # in case label was short enough, we still have a line break
      # at the end of the label. here we remove any trailing line breaks
      # -----------------------
      # get length of label
      l <- nchar(labels[n])
      # get last char
      lc <- substr(labels[n], l - lsub, l)
      # check if line break
      if (lc == ori.linesep) {
        # if yes, remove it
        labels[n] <- substr(labels[n], 0, l - (lsub + 1))
      }
    }
  }
  return(labels)
}### end -- TextWrap function
NULL





#' @encoding UTF-8
#' @title Convert All Factor Columns to Character Columns of a Data Frame
#'
#' @description By default, R converts character columns to factors.
#' Instead of re-reading the data using \code{stringsAsFactors}, the
#' \code{\link{Safechars}} function will identify which columns are currently factors, and convert them all to characters.
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}.
#'
#' @param .data a \code{data.frame}.
#' @seealso \code{\link{read.table}}, \code{\link{Destring}}.
#' @keywords internal
#' @examples
#'  str(iris)
#' iris_2 = Safechars(iris)
#' str(iris_2)
#'
#' @export
Safechars <- function(.data) {
  .data[sapply(.data, is.factor)] <-
    lapply(.data[sapply(.data, is.factor)], as.character)
  .data
}### end -- Safechars function
NULL




#' @title Split Data into Test and Train Sets
#' @description Split data given a vector \code{x} into two sets using a predefined ratio while preserving relative ratios of different labels in \code{x}. It is often used to split data for classification models into train and test subsets.
#' @param x Vector of data labels.
#' @param ratio The spliting ratio. Note that: if (0<=ratio<1), then \code{ratio} fraction of points from \code{x} will be set to \code{TRUE}. if (ratio==1) then one random point from \code{x} will be set to \code{TRUE}. if (ratio>1) then \code{ratio} number of points from \code{x} will be set to \code{TRUE}.
#' @param group Optional vector/list used when multiple copies of each sample are present.
#' @keywords manipulation
#'
#' @examples
#' data(titanic)
#' y = titanic[,4] # extract labels from the data
#' set.seed(88)
#' survived = data.split(y, ratio=3/5)
#' table(y,survived)
#'
#' t=sum( survived)  # n of elements in one class
#' f=sum( !survived) # n of elements in the other class
#' stopifnot( round((t+f)*3/5) == t) # test ratios
#'
#' # use results of data.split to subset data into train and test sets
#' train = subset(titanic, survived == TRUE)
#' test  = subset(titanic, survived == FALSE)
#'
#' @export
`data.split` <- function (x, ratio = 2/3, group = NULL)
{
  N = length(x)
  n = length(group)
  if (n > 0 && n != N)
    stop("Error in data.split: Vectors 'x' and 'group' have to have the same length")
  BinOne = logical(N)
  ratio = abs(ratio)
  if (ratio >= N)
    stop("Error in data.split: 'ratio' parameter has to be i [0, 1] range or [1, length(.data)] range")
  U = unique(x)
  nU = length(U)
  if (2 * nU > N | nU == 1) {
    nh = if (ratio >= 1)
      ratio
    else ratio * N
    rnd = runif(N)
    if (n)
      split(rnd, group) <- lapply(split(rnd, group), mean)
    ord = order(rnd)
    BinOne[ord[1:nh]] = TRUE
  }
  else {
    rat = if (ratio >= 1)
      ratio/N
    else ratio
    for (iU in 1:nU) {
      idx = which(x == U[iU])
      nh = round(length(idx) * rat)
      rnd = runif(length(idx))
      if (n) {
        grp = group[idx]
        split(rnd, grp) <- lapply(split(rnd, grp), mean)
      }
      ord = order(rnd)
      BinOne[idx[ord[1:nh]]] = TRUE
    }
  }
  if (ratio >= 1) {
    nh = sum(BinOne) - ratio
    if (nh > 0)
      BinOne[sample(which(BinOne), nh)] = FALSE
    else if (n < 0)
      BinOne[sample(which(!BinOne), -nh)] = TRUE
  }
  return(BinOne)
}
NULL





#' @title Clear Memory of All Objects
#'
#' @description This function is a wrapper for the command \code{rm(list=ls())}.
#'
#' @param obj The object (as a string) that needs to be removed (or kept)
#' @param keep Should \code{obj} be kept (i.e., everything but \code{obj} removed)? Or dropped?
#' @author Daniel Marcelino
#' @export
#' @examples
#' # create objects
#' a=1; b=2; c=3; d=4; e=5
#' # remove d
#' clear("d", keep=FALSE)
#' ls()
#' # remove all but a and b
#' clear(c("a", "b"), keep=TRUE)
#' ls()
`clear` = function(obj = NULL, keep = TRUE) {
  if (!is.null(obj)) {
    if (keep) {
      dropme = ls(envir = globalenv())[which(!(ls(envir = globalenv()) %in% obj))]
    } else {
      dropme = obj
    }
    rm(list = dropme, envir = globalenv())
    cat("All objects were deleted, except:", dropme, sep = ",")
  } else {
    rm(list = ls(envir = globalenv()), envir = globalenv())
    cat("All objects were deleted, including hidden package environments.\n")
  }
}### end -- clear function
NULL




#' @title Create useless hashes
#' @description A function to create useless hashes (####) for ease of commenting.
#' @param cols How many columns of hashes
#' @param rows How many rows of hashes
#' @author Daniel Marcelino
#' @keywords internal
#' @export
#' @examples
#' hash(cols=50, rows=1)
hash = function(cols=50, rows=2){
  k = paste(rep("#", times=cols),collapse="")
  for (m in 1:rows){
    cat("\n")
    cat(k)
  }
  cat("\n\n")
  for (m in 1:rows){

    cat(k)
    cat("\n")
  }
}
NULL




#' @title Odds Ratio for 2 x 2 Contingency Tables
#'
#' @description This function calculates the odds ratio for a 2 x 2 contingency table and a confidence interval (default conf.level is 95 percent) for the each estimate. x should be a matrix, data frame or table.
#'
#' @param x A 2 X 2 matrix, data frame or table of counts.
#' @param pad.zeros A logical. If \code{pad.zeros=TRUE}, zeros will be added to the matrix.
#' @param conf.level The confidence level of the interval.
#' @param \dots Additional arguements (currently ignored).
#'
#' @examples
#' tea <- matrix(c(3, 1, 1, 3), nrow = 2)
#' rownames(tea) <- c("milk", "tea")
#' colnames(tea) <- c("milk", "tea")
#' tea
#' odds.ratio(tea)
#'
#' mat <- matrix(c(18515, 18496, 1427, 1438), nrow = 2)
#' rownames(mat) <- c("Placebo", "Aspirin")
#' colnames(mat) <- c("No", "Yes")
#' mat
#' odds.ratio(mat)
#'
#' data(titanic)
#' tab = table(titanic$CLASS, titanic$SURVIVED)
#' tab
#' odds.ratio(tab)
#'
#' @rdname odds.ratio
#' @export
`odds.ratio` <- function(x, pad.zeros=FALSE, conf.level=0.95, ...) UseMethod("odds.ratio")

#' @rdname odds.ratio
#' @export
`odds.ratio` <-
  function(x, pad.zeros=FALSE, conf.level=0.95, ...) {
    if (pad.zeros) {
      if (any(x==0)) x <- x + 0.5
    }
    theta <- x[1,1] * x[2,2] / ( x[2,1] * x[1,2] )
    ASE <- sqrt(sum(1/x))
    CI <- exp(log(theta)
              + c(-1,1) * qnorm(0.5*(1+conf.level)) *ASE )
    list(estimator=theta,
         ASE=ASE,
         conf.interval=CI,
         conf.level=conf.level)
  }
NULL




#' @encoding UTF-8
#' @title Untable an Aggregated data.frame
#'
#' @description Method for recreate the data.frame out of a contingency table, i.e., converts from summarized data to long.
#' @param x The table object as a data.frame, table, or, matrix.
#' @param freq The column with count values.
#' @param rownames Row names to add to the data.frame.
#' @param \dots Extra parameters.
#'
#' @examples
#' gss <- data.frame(
#' expand.grid(sex=c("female", "male"),
#' party=c("dem", "indep", "rep")),
#' count=c(279,165,73,47,225,191))
#'
#' print(gss) # aggregated data.frame
#'
#' # Then expand it:
#' GSS <- untable(gss, freq="count")
#' head(GSS)
#'
#' @export
`untable` <- function(x, ...) {
  UseMethod("untable")
}
NULL


#' @rdname untable
#' @export
`untable.data.frame` <-
  function(x,
           freq = "Freq",
           rownames = NULL,
           ...) {
    if (all(is.na(match(freq, names(x)))))
      stop(gettextf("Frequency column %s does not exist!", freq))

    res <-
      x[untable(x[, freq], type = "as.numeric")[, ],-grep(freq, names(x))]
    rownames(res) <- rownames

    return(res)
  }
NULL



#' @rdname untable
#' @param dimnames Set dimnames of an object if require.
#' @param type The type of variable. If NULL, ordered factor is returned.
#' @param colnames Column names to add to the data.frame.
#' @export
`untable.default` <-
  function(x,
           dimnames = NULL,
           type = NULL,
           rownames = NULL,
           colnames = NULL,
           ...) {
    # coerce to table, such as also be able to handle vectors
    x <- as.table(x)
    if (!is.null(dimnames))
      dimnames(x) <- dimnames
    if (is.null(dimnames) &&
        identical(type, "as.numeric"))
      dimnames(x) <- list(seq_along(x))
    # set a title for the table if it does not have one
    # if(is.null(names(dimnames(x)))) names(dimnames(x)) <- ""
    # if(length(dim(x))==1 && names(dimnames(x))=="") names(dimnames(x)) <- "Var1"
    # replaced 26.3.2013
    for (i in 1:length(dimnames(x)))
      if (is.null(names(dimnames(x)[i])) ||
          names(dimnames(x)[i]) == "")
        if (length(dimnames(x)) == 1)
          names(dimnames(x)) <- gettextf("Var%s", i)
        else
          names(dimnames(x)[i]) <- gettextf("Var%s", i)

        res <-
          as.data.frame(expand.grid(dimnames(x))[rep(1:prod(dim(x)), as.vector(x)), ])
        rownames(res) <- NULL
        if (!all(names(dimnames(x)) == ""))
          colnames(res) <- names(dimnames(x))

        # return ordered factors, if wanted...
        if (is.null(type))
          type <- "as.factor"
        # recycle type:
        if (length(type) < ncol(res))
          type <- rep(type, length.out = ncol(res))

        for (i in 1:ncol(res)) {
          if (type[i] == "as.numeric") {
            res[, i] <- as.numeric(as.character(res[, i]))
          } else {
            res[, i] <- eval(parse(text = gettextf("%s(res[,i])", type[i])))
          }
        }

        # overwrite the dimnames, if requested
        if (!is.null(rownames))
          rownames(res) <- rownames
        if (!is.null(colnames))
          colnames(res) <- colnames
        class(res) <- c("SciencesPo", "untable", "data.frame")
        return(res)
  }### end -- untable function
NULL




#' @title Look for Variables in a Data Frame Using Regular Expressions
#' @description The function look for variable names in a \code{data.frame}.
#' @param .data a data.frame object.
#' @param varnames a character or vector with regular expressions of variables names.
#' @return Returns the variables names of the variables found.
#' @examples
#' LookVariable(titanic, "clas")
#'
LookVariable  <- function(.data, varnames) {
  n  <- names(.data)
  nn  <- list()
  for (i in 1:length(varnames)) {
    nn[[i]]  <- grep(varnames[i],n, ignore.case = TRUE)
  }

  nn  <- unlist(nn)

  if ( length(nn) >0 )
  {
    r  <- n[nn]
    return(r)
  }
  else
  { return("No variables found")}
}
NULL





#' @title Count missing data.
#' @description Function to count missing data.
#' @param .data The \code{data.frame}.
#' @param vars a vector with name of variables.
#' @param share logical. If \code{TRUE}, it will show the share of missing data by variable.
#' @param exclude.complete logical. Exclude complete variables from the output.
#' @export
#' @examples
#' library(data.table)
#'
#' a <- c(NA,2,3,4, NA)
#' b <- c(2,3,4,NA, 3)
#' c <- c(1,2,3,1, 1)
#' data <- data.table(a,b,c)
#' CountMissing(data, share = TRUE)
#'
CountMissing  <- function(.data, vars = NULL, share = FALSE, exclude.complete = TRUE) {

  if (is.null(vars)) {
    vars <- names(.data)
  }

  mis <- sort( sapply(.data[, vars, with = FALSE], function(x) sum(is.na(x))), decreasing = TRUE)

  if (exclude.complete == TRUE) {
    mis <- mis[ mis > 0]
  }

  if (share == FALSE)
  { return(mis) }

  else if ( share == TRUE )
  { return( round(mis / nrow(.data), 3)) }

  return(mis)

}
NULL







#' Text-based File Interface
#'
#' Text-based file interface.
#'
#' @param root the root directory to look in
#' @param multiple logical. enable multiple files?
#' @source
#'  \url{http://stackoverflow.com/questions/9122600/r-command-line-file-dialog-similar-to-file-choose}
#'
#' @export
#' @examples
#' if (interactive()) {
#' file.browser()
#' 	}
#'
file.browser <- function (root = getwd(), multiple = FALSE) {
  # .. and list.files(root)
  x <- c(dirname(normalizePath(root)), list.files(root,full.names = TRUE) )
  isdir <- file.info(x)$isdir
  obj <- sort(isdir,index.return = TRUE, decreasing = TRUE)
  isdir <- obj$x
  x <- x[obj$ix]
  lbls <- sprintf('%s%s',basename(x),ifelse(isdir,'/',''))
  lbls[1] <- sprintf('../ (%s)', basename(x[1]))

  files <- c()
  sel = -1
  while (TRUE) {
    txt <- sprintf('Select file(s) (0 to quit) in folder %s:', root)
    sel <- menu(lbls, title = txt)
    if (sel == 0)
      break
    if (isdir[sel]) {
      # directory, browse further
      files <- c(files, file.browser( x[sel], multiple ))
      break
    } else {
      # file, add to list
      files <- c(files, x[sel])
      if (!multiple)
        break
      # remove selected file from choices
      lbls <- lbls[-sel]
      x <- x[-sel]
      isdir <- isdir[-sel]
    }
  }
  return(files)
}
NULL





#' Interactively order columns in ggplot
#'
#' Order a categorical variable by the values in a numerical variable in ggplot
#' @param data data frame
#' @param axis categorical axis
#' @param column numerical variable by which to order
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @export
#' @examples
#' ggplot(orderedAxis(mtcars, gear, carb),
#'        aes(x = gear.o, y = carb))
#'
orderedAxis<-function(data, axis, column)
{
  # for interactivity with ggplot2
  arguments <- as.list(match.call())
  #if tidy information with more than 1 column
  #if(length(names(data))>2)
  #{
  # data <- data %>%
  #  dplyr::group_by(eval(arguments$axis)) %>%
  # dplyr::summarise(newsum = sum(column))
  #  data <- as.data.frame(data)
  # names(data)<-c(as.character(arguments$axis), as.character(arguments$column))
  #} else {}

  col <- eval(arguments$column, data)
  ax <- eval(arguments$axis, data)

  # evaluated factors
  a<-reorder(with(data, ax),
             with(data, col))

  #new_data
  df<-cbind.data.frame(data)
  # define new var
  within(df,
         do.call("<-",list(paste0(as.character(arguments$axis),".o"), a)))
}
NULL




##' Generate a sequence of column labels to match Excel's naming
##'
##' Excel columns are labeled with letters (e.g., A, B, C, ... AA, AB, AC, etc). Given an integer
##' (n), this function will return labels starting from A until the nth column. See examples.
##' @title Generate Excel column labels
##' @param n an integer that indicates how many named columns the user wishes to obtain
##' @return a vector of strings of length n
##' @author Dustin Fife
##' @seealso \code{\link{excelMatch}}
##' @examples
##' excelCols(11)
##' @export
excelCols=function(n){
  vec1=LETTERS
  if(n<=26){
    res=vec1[seq_len(n)]
  } else if(n>26&n<=702){
    res=c(vec1,apply(expand.grid(vec1,vec1)[,2:1],1,paste,collapse=""))[1:n]
  } else if(n>702&n<=18278){
    res=c(vec1,apply(expand.grid(vec1,vec1)[,2:1],1,paste,collapse=""),apply(expand.grid(vec1,vec1,vec1)[,3:1],1,paste,collapse=""))[1:n]
  } else{
    res=NA###forgot
  }
  res
}

##' Obtain column number or variable name from Excel named Columns
##'
##' Excel columns are labeled with letters (e.g., A, B, C, ... AA, AB, AC, etc). This function
##' accepts a string (e.g., "AAC") and returns either a number that indicates where that string falls
##' in the sequence of excel named columns, or it returns the variable name corresponding to that column number.
##'
##' @title Obtain column number or variable name from Excel named Columns
##' @param ... An Excel-like string consisting of all capital letters (e.g., "AAQ", "BZ", "RQ", "S", etc.)
##' @param n the number of columns of the data of interest
##' @param names the column names of the data of interest
##' @return either a variable name or column number
##' @seealso \code{\link{excelCols}}
##' @examples
##' fake.names = paste("Variable", 1:1000, sep="")
##' # find the Variable name corresponding to AC
##' excelMatch("AC", names=fake.names)
##' # find the column number instead
##' excelMatch("AC", n=1000)
##' @author Dustin Fife
##' @export
excelMatch = function(..., n=NULL, names=NULL){

  #### check for errors
  if (is.null(n) & is.null(names)){
    stop("You must supply either an n value (integer) or a vector of names(names)")
  } else if (!is.null(names)){
    n = length(names)
  }

  #### generate excel columns
  excelColumns = excelCols(n)

  #### do the search
  findit = which(excelColumns %in% unlist(list(...)))

  if (!is.null(names)){
    return(names[findit])
  } else {
    return(findit)
  }
}
NULL




#' @title Computes Weighted Mean
#' @description Computes a weighted mean of data.
#' @param x A numeric vector.
#' @param weights A numeric vector of weights of \code{x}.
#' @param normwt Ignored.
#' @param na.rm A logical, if \code{TRUE}, missing data will be dropped.  If \code{na.rm = FALSE}, missing data will return an error.
#' @export
#' @examples
#' x <- sample(10,10)
#' w <- sample(5,10, replace=TRUE)
#'
#' scpo.wtd.mean(x, w)
`scpo.wtd.mean` <- function (x, weights = NULL, normwt = "ignored", na.rm = TRUE)
{
  if (!length(weights))
    return(mean(x, na.rm = na.rm))
  if (na.rm) {
    s <- !is.na(x + weights)
    x <- x[s]
    weights <- weights[s]
  }
  sum(weights * x)/sum(weights)
}
NULL



#' @title Weighted Frequency Table
#'
#' @description Computes a weighted frequency table, only one stratification variable is supported.
#'
#' @param x A numeric vector, may be a character or category or factor vector.
#' @param weights A numeric vector of weights.
#' @param type The default type is "list", meaning that the function is to return a list containing two vectors: x is the sorted unique values of x and sum.of.weights is the sum of weights for that x. This is the default so that you don't have to convert the names attribute of the result that can be obtained with type="table" to a numeric variable when x was originally numeric. type="table" for wtd.table results in an object that is the same structure as those returned from table.
#'
#' @param normwt specify normwt=TRUE to make weights sum to length(x) after deletion of NAs. If weights are frequency weights, then normwt should be FALSE, and if weights are normalization (aka reliability) weights, then normwt should be TRUE. In the case of the former, no check is made that weights are valid frequencies.
#' @param na.rm A logical, if \code{TRUE}, missing data will be dropped.  If \code{na.rm = FALSE}, missing data will return an error.
#'
#' @examples
#'  x <- sample(5,25, replace=TRUE)
#'  w <- sample(3,25, replace=TRUE)
#'
#' scpo.wtd.table(x, w)
#'
#' @export
`scpo.wtd.table` <- function (x, weights = NULL, type = c("list", "table"), normwt = FALSE, na.rm = TRUE)
{
  type <- match.arg(type)
  if (!length(weights))
    weights <- rep(1, length(x))
  isdate <- scpo.isDate(x)
  ax <- attributes(x)
  ax$names <- NULL
  if (is.character(x))
    x <- as.factor(x)
  lev <- levels(x)
  x <- unclass(x)
  if (na.rm) {
    s <- !is.na(x + weights)
    x <- x[s, drop = FALSE]
    weights <- weights[s]
  }
  n <- length(x)
  if (normwt)
    weights <- weights * length(x)/sum(weights)
  i <- order(x)
  x <- x[i]
  weights <- weights[i]
  if (anyDuplicated(x)) {
    weights <- tapply(weights, x, sum)
    if (length(lev)) {
      levused <- lev[sort(unique(x))]
      if ((length(weights) > length(levused)) && any(is.na(weights)))
        weights <- weights[!is.na(weights)]
      if (length(weights) != length(levused))
        stop("program logic error")
      names(weights) <- levused
    }
    if (!length(names(weights)))
      stop("program logic error")
    if (type == "table")
      return(weights)
    x <- scpo.AllIsNumeric(names(weights), "vector")
    if (isdate)
      attributes(x) <- c(attributes(x), ax)
    names(weights) <- NULL
    return(list(x = x, sum.of.weights = weights))
  }
  xx <- x
  if (isdate)
    attributes(xx) <- c(attributes(xx), ax)
  if (type == "list")
    list(x = if (length(lev)) lev[x] else xx, sum.of.weights = weights)
  else {
    names(weights) <- if (length(lev))
      lev[x]
    else xx
    weights
  }
}
NULL






#' @title Computes Weighted Proportions
#' @description Computes a weighted proportions table of data in each category for any variable.
#' @param x A vector for which a set of proportions is desired.
#' @param weights A vector of weights to be used to determining the weighted proportion in each category of \code{x}.
#' @param na.rm A logical, if \code{TRUE}, missing data will be dropped.  If \code{na.rm = FALSE}, missing data will return an error.
#'
#' @examples
#' x <- sample(10,10)
#' w <- sample(5,10, replace=TRUE)
#'
#' scpo.wtd.prop(x, w)
#'
#' @export
`scpo.wtd.prop` <- function(x, weights=NULL, na.rm=TRUE){
  if(is.null(weights)){
    weights <- rep(1, length(x))
  }
  y <- scpo.wtd.table(x, weights, na.rm=na.rm)$sum.of.weights/sum(scpo.wtd.table(x, weights, na.rm=na.rm)$sum.of.weights)
  z <- as.vector(y)
  names(z) <- names(y)
  if(is.logical(x))
    z <- rev(z)
  z
}
NULL





#' @title Weighted Variance
#'
#' @description Weighted Variance Formula
#'
#' @param x the variable.
#' @param w the variance.
#' @param na.rm A logical if NA should be disregarded.
#' @keywords Stats
#' @export
#' @examples
#' wt=c(1.23, 2.12, 1.23, 0.32, 1.53, 0.59, 0.94, 0.94, 0.84, 0.73)
#' x = c(5, 5, 4, 4, 3, 4, 3, 2, 2, 1)
#' scpo.wtd.var(x, wt)
`scpo.wtd.var` <- function(x, w, na.rm = FALSE) {
  if (na.rm) {
    w <- w[i <- !is.na(x)]
    x <- x[i]
  }
  sum.w <- sum(w)
  sum.w2 <- sum(w^2)
  mean.w <- sum(x * w) / sum(w)
  (sum.w / (sum.w^2 - sum.w2)) * sum(w * (x - mean.w)^2, na.rm = na.rm)
}
NULL





#' @title Computes Weighted Standardized Values
#' @description Computes weighted standardized values of data in each category for any variable.
#' @param x A vector for which a set of standardized values is desired.
#' @param weights A vector of weights to be used to determining the weighted values of \code{x}.
#'
#' @examples
#'  x <- sample(10,10)
#'  w <- sample(5,10, replace=TRUE)
#'
#' scpo.wtd.stdz(x, w)
#' @export
`scpo.wtd.stdz` <- function(x, weights=NULL){
  if(is.null(weights)){
    weights <- rep(1, length(x))
  }
  x <- x-scpo.wtd.mean(x, weights, na.rm=TRUE)
  x <- x/sqrt(scpo.wtd.var(x, weights, na.rm=TRUE))
  x
}
NULL




#' @title Computes Weighted Correlations
#' @description Computes weighted correlations.
#' @useDynLib SciencesPo
#' @export
#' @param x A matrix or vector to correlate with \code{y}.
#' @param y A matrix or vector to correlate with \code{x}. If \code{y} is NULL, \code{x} will be used instead.
#' @param weights An optional vector of weights to be used to determining the weighted mean and variance for calculation of the correlations.
#'
#' @examples
#'  x <- sample(10,10)
#'  y <- sample(10,10)
#'  w <- sample(5,10, replace=TRUE)
#'
#' scpo.wtd.corr(x, y, w)
#'
`scpo.wtd.corr` <- function(x, y=NULL, weights=NULL){
  if(is.null(y)){
    y <- x
  }
  q <- as.matrix(x)
  r <- as.matrix(y)
  if(is.null(weights)){
    weights <- rep(1, dim(q)[1])
  }
  x <- q[!is.na(weights),]
  y <- r[!is.na(weights),]
  weights <- weights[!is.na(weights)]
  out <- .Call("wcorr", as.matrix(x), as.matrix(y), as.double(weights), NAOK=TRUE, PACKAGE="SciencesPo")
  ## C code for this package was contributed by Marcus Schwemmle
  if(!is.null(colnames(x)))
    rownames(out) <- colnames(x)
  if(!is.null(colnames(y)))
    colnames(out) <- colnames(y)
  out
}
NULL







#' Summarize Missingness Across a data frame
#'
#' This function calculates the number and percent missing for every variable in .data.
#'
#' @param .data The dataset or vector.
#' @param order A logical, should results be ordered by degree of missingness? Default is \code{order=TRUE}.
#' @examples
#'
#' foo <- data.frame(X1 = rnorm(100),
#'                   X2 = sample(c(NA, 'A', 'B', 'C'), 100, TRUE),
#'                   X3 = sample(c('A', NA, 'C', NA), 100, TRUE))
#'
#' prop.miss(foo)
#'
#' @export
#'
`prop.miss` <- function(.data, order = TRUE) {
  # total no. of obs.
  n <- nrow(.data)
  # no. that are missing
  nmiss <- apply(.data, MARGIN = 2, FUN = function(x){
    # we include both NAs and blanks as missing values
    return(sum(is.na(x) | x == ''))
  })
  # variable names
  vars <- names(nmiss)
  # number formatting
  format <- function(x){
    return(prettyNum(x, big.mark = ',', scientific = FALSE))
  }
  # output table
  out <- data.frame(
    variable = vars,
    missfrac = paste0(format(nmiss), '/', format(n)),
    missperc = paste0(round(nmiss/n * 100, 2), '%')
  )
  rownames(out) <- NULL
  colnames(out) <- c('var', 'missing/total', 'missing')
  if(order){
    return(out[order(nmiss, decreasing = TRUE),])
  } else {
    return(out)
  }
}
NULL






#' @title  Return only one row per ID
#'
#' @description If an individual has multiple observations in the dataset, \code{last.entry} will
#' loop through the entire dataset and return only one observation per individual, giving the
#' first (or last) draw for a person.
#'
#' @param ID The name of the unique identifier, expressed as a string (e.g., "Match.Group")
#' @param sort.var The variable to be sorted on in order to take the first (or last) sample, expressed as a string.
#' @param decreasing How should the sort.var variable be sorted? Defaults to TRUE.
#' @param data The dataset with multiple rows per individual
#' @param FUN What should be done with the multiple samples? This function can be used to extract the last
#' (or first) sample using the decreasing/sort.var options, or a function can be performed (such as the mean)
#' on one or more columns. See examples.
#' @param fun.var IF not \code{NULL}, the variable (or a vector of variables), expressed as strings to have the function
#' applied to.
#' @param \dots Additional arguements (currently ignored).
#'
#'
#' @return a new dataframe containing one row per ID
#' @author Daniel Marcelino
#'
#' @export
#' @examples
#' # take only group 2 values
#' last.entry(ID="ID", sort.var="group", data=sleep)
#'
#' # take only group 1 values
#' last.entry(ID="ID", sort.var="group", decreasing=FALSE,data=sleep)
#'
#' # average group 1 and 2 values
#' last.entry(ID="ID", data=sleep, FUN=mean, fun.var="extra")
#'
#' # take the maximum extra value
#' last.entry(ID="ID", data=sleep, FUN=max, fun.var="extra")
#'
#' # take the mean of two columns extra value
#' sleep$group = as.numeric(as.character(sleep$group))
#' last.entry(ID="ID", data=sleep, FUN=mean, fun.var=c("group","extra"))
#'
`last.entry` = function(ID, sort.var=NULL, decreasing=TRUE, data, FUN=NULL, fun.var=NULL,...){

  #### if they gave a function but not a variable (or vice versa), bark
  if (is.null(FUN) & !is.null(fun.var) | !is.null(FUN) & is.null(fun.var)){
    stop("Both FUN and fun.var must either be null or not null.")
  }

  #### first sort by specified variable
  if (!is.null(sort.var)){
    data = data[order(data[,ID],data[,sort.var], decreasing=decreasing),]
  }

  # extract unique IDs
  IDs = unique(data[,ID])

  # preallocate
  new.dat = data[1:length(IDs),]

  # loop
  for (i in 1:nrow(new.dat)){
    k = data[data[,ID]==IDs[i],]

    new.dat[i,] = k[1,]

    # if they specified a FUN, use it on the variable specified
    if (is.null(fun.var)){
      new.dat[i,fun.var] = k[1,]
    } else if (length(fun.var)==1){
      new.dat[i,fun.var] = FUN(k[,fun.var],...)
    } else {
      new.dat[i,fun.var] = apply(k[,fun.var], 2, FUN=FUN,...)
    }
  }
  return(new.dat)
}
NULL



#' @title Replace Character
#' @description  Replaces characters in a given string.
#' @param str The target string.
#' @param char The replacing character.
#' @param newchar The new character.
#'
#' x <- "replace_strings"
#'
#' replace.char(x, char = "_", newchar = ".")
#'
#' @export
`replace.char` <- function(str, char = "_", newchar = ".")
{  ## tjoelker@redwood.rt.cs.boeing.com (Rod Tjoelker 865-3197)
  under <- grep(char, str)
  for(i in under) {
    nc <- nchar(str[i])
    ch <- substring(str[i], 1:nc, 1:nc)
    ch <- ifelse(ch == char, newchar, ch)
    str[i] <- paste(ch, collapse = "")
  }
  return(str)
}
NULL




kruskalmc <- function (resp,...) {
  UseMethod("kruskalmc")
}


kruskalmc.default <- function (resp, categ, probs = 0.05, cont = NULL,...)
{
  db<-na.omit(data.frame(resp,categ))
  if(nrow(db)!=length(resp)) warning(paste(length(resp)-nrow(db),"lines including NA have been omitted"))
  resp<-db[,1]
  categ<-db[,2]
  lst <- split(rank(resp), categ)
  name <- names(lst)
  R <- sapply(lst, mean)
  n <- sapply(lst, length)
  N = length(resp)
  dif <- abs(outer(R, R, "-"))
  if (is.null(cont)) {
    difv <- NULL
    vname <- NULL
    indices <- NULL
    for (i in 1:(length(name) - 1)) {
      for (j in (i + 1):length(name)) {
        vname <- c(vname, paste(name[i], "-", name[j], sep = ""))
        indices <- rbind(indices, c(i, j))
        difv<-c(difv,dif[i,j])
      }
    }
    names(difv) <- vname
    z <- qnorm(probs/(length(lst) * (length(lst) - 1)), lower.tail = FALSE)
    lims <- z * sqrt((N * (N + 1)/12) * (1/n[indices[1:length(vname),1]] + 1/n[indices[1:length(vname), 2]]))
    names(lims) <- vname
    stat <- "Multiple comparison test after Kruskal-Wallis"
  }
  else {
    vname = NULL
    indices = NULL
    for (j in 2:length(dif[1, ])) {
      vname <- c(vname, paste(name[1], "-", name[j], sep = ""))
      indices <- rbind(indices, c(1, j))
    }
    dif <- dif[1, 2:length(dif[1, ])]
    names(dif) <- vname
    difv<-dif
    choice <- pmatch(cont, c("two-tailed","one-tailed"), nomatch = 3)
    if (choice == 1) {
      z <- qnorm(probs/(2 * (length(lst) - 1)), lower.tail = FALSE)
      lims <- z * sqrt((N * (N + 1))/12 * (1/n[indices[1:length(vname),
                                                       1]] + 1/n[indices[1:length(vname), 2]]))
      names(lims) <- vname
      stat <- "Multiple comparison test after Kruskal-Wallis, treatments vs control (two-tailed)"
    }
    if (choice == 2) {
      z <- qnorm(probs/(length(lst) - 1), lower.tail = FALSE)
      lims <- z * sqrt((N * (N + 1)/12) * (1/n[indices[1:length(vname),
                                                       1]] + 1/n[indices[1:length(vname), 2]]))
      names(lims) <- vname
      stat <- "Multiple comparison test after Kruskal-Wallis, treatment vs control (one-tailed)"
    }
    if (choice == 3)
      stop("Values must be 'one-tailed' or 'two-tailed', partial matching accepted")
  }
  output <- list(statistic = stat, signif.level = probs, dif.com = data.frame(obs.dif = difv,
                                                                              critical.dif = lims, difference = ifelse((difv - lims) > 0, TRUE, FALSE)))
  class(output) <- c("mc", "list")
  output
}


kruskalmc.formula <- function(resp,data=NULL,...) {
  mf <- model.frame(resp,data)
  kruskalmc.default(mf[,1],mf[,2],...)
}
NULL



sci.notation <- function(x, digits = 1) {
  if (length(x) > 1) {
    return(append(sci_notation(x[1]), sci_notation(x[-1])))
  }
  if (!x) return(0)
  exponent <- floor(log10(x))
  base <- round(x / 10^exponent, digits)
  if (sum(base == 1) == length(base)) {
    as.expression(substitute(10^exponent,
                             list(exponent = exponent)))
  } else {
    as.expression(substitute(base %*% 10^exponent,
                             list(base = base, exponent = exponent)))
  }

}
NULL



"procfreq" <-
  function(x, ...) {UseMethod("procfreq")}

"procfreq.default" <-
  function(x, digits=4) {
    total <- sum(x)
    rowsum <- apply(x,1,sum)
    colsum <- apply(x,2,sum)
    prop <- x/total
    rowprop <- sweep(x,1,rowsum,"/")
    colprop <- sweep(x,2,colsum,"/")
    expected <- (matrix(rowsum) %*% t(matrix(colsum))) / total
    dimnames(expected) <- dimnames(x)
    resid <- (x-expected)/sqrt(expected)
    adj.resid <-
      resid/sqrt((1-matrix(rowsum)/total) %*% t(1-matrix(colsum)/total))
    df <- prod(dim(x)-1)
    X2 <- sum(resid^2)
    attr(X2,"P-value") <- 1-pchisq(X2,df)
    ## Must be careful about zero freqencies.  Want 0*log(0) = 0.
    tmp <- x*log(x/expected)
    tmp[x==0] <- 0
    G2 <- 2 * sum(tmp)
    attr(G2,"P-value") <- 1-pchisq(G2,df)
    list(sample.size=total,
         row.totals=rowsum,
         col.totals=colsum,
         overall.proportions=prop,
         row.proportions=rowprop,
         col.proportions=colprop,
         expected.freqs=expected,
         residuals=resid,
         adjusted.residuals=adj.resid,
         chi.square=X2,
         likelihood.ratio.stat=G2,
         df=df)
  }



adj.residuals <-
  function(fit, ...) {
    residuals(fit, ...) / sqrt(1 - lm.influence(fit)$hat)
  }
