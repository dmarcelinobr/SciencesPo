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
