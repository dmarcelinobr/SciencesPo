#' @encoding UTF-8
#' @title Untable an Aggregated Data Frame
#'
#' @description A method for recovering a \code{data.frame} out of summarized data, i.e.  contingency table or aggregated data.
#' @param x the table object as a data.frame, table, or, matrix.
#' @param freq the column name of count values.
#' @param row.names row names to add to \code{data.frame} if any.
#' @param \dots Extra parameters ignored.
#' @seealso \code{\link{expand.grid}}, \code{\link{gl}}.
#' @examples
#' gss <- data.frame(
#' expand.grid(sex=c("female", "male"),
#' party=c("dem", "indep", "rep")),
#' count=c(279,165,73,47,225,191))
#'
#' print(gss) # aggregated data.frame
#'
#' # Then expand it:
#' GSS <- Untable(gss, freq="count")
#' head(GSS)
#'
#' # Expand from a table or xtable object:
#' # Fisher's Tea-Tasting Experiment data
#'  tea <- table(poured=c("Yes", "Yes", "Yes", "No","Yes","No", "No", "No"),
#'              guess=c("Yes", "Yes", "Yes", "Yes", "No", "No","No","No"))
#'
#' Untable(tea)
#'
#' # Expand with a vector of weights
#' Untable(c(3,3,3), dimnames=list(c("Brazil","Colombia","Argentina")))
#'
#' @rdname Untable
#' @export
`Untable` <- function(x, ...) {
  UseMethod("Untable")
}
NULL

#' @rdname Untable
#' @export
`Untable.data.frame` <-
  function(x,
           freq = "Freq",
           row.names = NULL,
           ...) {
    if (all(is.na(match(freq, names(x)))))
      stop(gettextf("Frequency column %s does not exist!", freq))
    res <-
      x[Untable(x[, freq], type = "as.numeric")[, ],-grep(freq, names(x))]
    rownames(res) <- row.names
    class(res) <- c("Untable", "data.frame")
    return(res)
  }
NULL



#' @param dimnames set the dimnames of object if required.
#' @param type the type of variable. If NULL, ordered factor is returned.
#' @param col.names column names to add to the data.frame.
#' @rdname Untable
#' @export
`Untable.default` <-
  function(x,
           dimnames = NULL,
           type = NULL,
           row.names = NULL,
           col.names = NULL,
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
        row.names(res) <- NULL
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
        if (!is.null(row.names))
          rownames(res) <- row.names
        if (!is.null(colnames))
          colnames(res) <- col.names
        class(res) <- c("Untable", "data.frame")
        return(res)
  }### end -- untable function
NULL
