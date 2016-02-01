#' @title Chain operator
#' @description Chain operator.
#' @name %>%
#' @export %>%
#' @keywords manipulation
#' @rdname chain
#' @usage x %>% f(y) is translated into f(x, y).
`%>%` <- magrittr::`%>%`



#' @encoding UTF-8
#' @title Make Proper Case
#' @description Simply replaces all caps strings into proper case strings.
#' @param x is a character vector.
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @export
#' @examples
#' asTitleCase("THIS SHOULDN'T BE ALL CAPITAL LETTERS")
#'
`asTitleCase` <- function(x) {
  return (gsub("\\b([A-Z])([A-Z]+)", "\\U\\1\\L\\2" , x, perl=TRUE))
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
#' @title Insert line breaks in long strings
#' @name textwrap
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
#' @examples
#' textwrap(c("A very long string", "And another even longer string!"), 10)
#'
#' @export
textwrap <- function(labels, wrap, linesep = NULL) {
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
}### end -- textwrap function
NULL





#' @encoding UTF-8
#' @title  Convert All Factor Columns to Character Columns
#'
#' @description By default, R converts character columns to factors.
#' Instead of re-reading the data using \code{stringsAsFactors}, the
#' \code{\link{safe.chars}} function will identify which columns are currently factors, and convert them all to characters.
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}.
#'
#' @param .data The name of the \code{data.frame}
#' @seealso \code{\link{read.table}}, \code{\link{destring}}.
#' @examples
#'  str(iris)
#' iris_2 = safe.chars(iris)
#' str(iris_2)
#'
#' @export
safe.chars <- function(.data) {
  .data[sapply(.data, is.factor)] <-
    lapply(.data[sapply(.data, is.factor)], as.character)
  .data
}### end -- safe.chars function
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


##' A function to create useless hashes (####) for ease of commenting
##'
##' @title Create useless hashes
##' @param j How many columns of hashes
##' @param i How many rows of hashes
##' @author Dustin Fife
##' @export
##' @examples
##' hash(j=100, i=11)
hash = function(j=50, i=4){
  k = paste(rep("#", times=j),collapse="")
  for (m in 1:i){
    cat("\n")
    cat(k)
  }
  cat("\n\n")
  for (m in 1:i){

    cat(k)
    cat("\n")
  }
}
NULL



##' Return only one row per ID
##'
##' Often times, an individual has multiple observations in the dataset. \code{last.sample} will
##' loop through the entire dataset and return only one observation per individual, giving the
##' first (or last) draw for a person, or performing some function on the variable of interest.
##'
##' @param ID The name of the unique identifier, expressed as a string (e.g., "Match.Group")
##' @param sort.var The variable to be sorted on in order to take the first (or last) sample, expressed as a string.
##' @param decreasing How should the sort.var variable be sorted? Defaults to T.
##' @param data The dataset with multiple rows per individual
##' @param FUN What should be done with the multiple samples? This function can be used to extract the last
##' (or first) sample using the decreasing/sort.var options, or a function can be performed (such as the mean)
##' on one or more columns. See examples.
##' @param If FUN if not null, the variable (or a vector of variables), expressed as strings to have the function
##' applied to.
##' @aliases lastsample lastSample one.row last.row
##' @return a new dataframe containing one row per ID
##' @author Dustin Fife
##' @export
##' @examples
##' #### take only group 2 values
##' last.sample(ID="ID", sort.var="group", data=sleep)
##' #### take only group 1 values
##' last.sample(ID="ID", sort.var="group", decreasing=FALSE,data=sleep)
##' #### average group 1 and 2 values
##' last.sample(ID="ID", data=sleep, FUN=mean, fun.var="extra")
##' #### take the maximum extra value
##' last.sample(ID="ID", data=sleep, FUN=max, fun.var="extra")
##' #### take the mean of two columns extra value
##' sleep$group = as.numeric(as.character(sleep$group))
##' last.sample(ID="ID", data=sleep, FUN=mean, fun.var=c("group","extra"))
last.sample = function(ID, sort.var=NULL, decreasing=TRUE, data, FUN=NULL, fun.var=NULL,...){

  #### if they gave a function but not a variable (or vice versa), bark
  if (is.null(FUN) & !is.null(fun.var) | !is.null(FUN) & is.null(fun.var)){
    stop("Both FUN and fun.var must either be null or not null.")
  }

  #### first sort by specified variable
  if (!is.null(sort.var)){
    data = data[order(data[,ID],data[,sort.var], decreasing=decreasing),]
  }

  #### extract unique IDs
  IDs = unique(data[,ID])

  #### preallocate
  new.dat = data[1:length(IDs),]

  #### loop
  for (i in 1:nrow(new.dat)){
    k = data[data[,ID]==IDs[i],]

    new.dat[i,] = k[1,]

    ### if they specified a FUN, use it on the variable specified
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



##' Extract only part of a string, given a separator
##'
##' Given a string with a separator (e.g., "Subject 001", where the separator is a space), this function can be used to extract only whatever follows the separator (in this case, "001").
##' It is often used when data comes in with a conglomorated identifier (such as case-matchNumber-drawNumber-Month).
##' @title Extract only part of a string
##' @param string a vector of strings
##' @param sep the separator that separates the parts of the strings
##' @param position the position of the element you wish to extract
##' @return the element the user wishes to extract
##' @author Dustin Fife
##' @export
##' @examples
##' barcode = c("Case-001-B", "Control-001-A", "Case-002-A")
##' subsetString(barcode, sep="-", position=2)
##' subsetString(barcode, sep="-", position=3)
subsetString = function(string, sep=" ", position=3){
  string = unlist(lapply(string, FUN=function(x){unlist(strsplit(x, sep, fixed=TRUE))[position]}))
  string
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
