#' @title Frequency table
#' @description Computes absolute and relative frequencies of a vector x. Continuous (numeric) variables are cut as the \code{\link{hist}} function, whereas categorical variables are aggregated by \code{\link{table}}.
#'
#' @param x The variable to be described.
#' @param breaks Either a numeric vector of two or more cut points or a integer (> 2) for the intervals. If not provided, the default is taken from \code{hist()}. This parameter is ignored if x is not of class numeric.
#' @param digits An integer for the decimals.
#' @param include.lowest A logical, indicating if an x[i] equal to the lowest (or highest, for \code{right = FALSE}) \code{"breaks"} value should be included. Ignored if x is not of numeric type.
#' @param ord How should the result be ordered. Default is \code{"level"}, other choices are by frequency: (\code{"desc"} or \code{"asc"}) or 'by name of the levels' (\code{"name"}).
#' @param useNA A character parameter to be passed to \code{table}.
#' @param \dots Further arguments are: \code{\link{cut}()}, and \code{right} to define if the intervals should be closed on the right (and open on the left) or vice versa.
#'
#' @importFrom graphics hist
#' @examples
#' data(ssex)
#' with(ssex, freq(Favor))
#'
#' # sorted by frequency
#' freq(ssex$Favor, ord="desc")
#'
#' # sorted by name, including NAs
#' with(ssex, freq(Favor, ord="name", useNA="ifany"))
#'
#' @export
`freq` <-
  function(x, breaks = hist(x, plot = FALSE)$breaks, digits=2, include.lowest = TRUE, ord = c("level", "desc", "asc", "name"),
           useNA = c("no", "ifany", "always"), ...){

    # check if x is a vector (do not use is.vector())
    if(!(is.atomic(x) || is.list(x))) stop("'x' must be a vector")

    if(inherits(x, "table")){
      tab <- x

    } else {

      if(is.numeric(x)){
        x <- base::cut(x, breaks = breaks, include.lowest = include.lowest, ordered_result = TRUE, ...)
      }

      tab <- base::table(x, useNA = useNA)
    }

    # how should the table be sorted, by name, level or frq? (NULL means "desc")
    switch(match.arg(ord, c("level", "desc", "asc", "name")),
           level  = {  }
           , name   = { tab <- tab[rownames(tab)] }
           , asc    = { tab <- sort(tab) }
           , desc   = { tab <- -sort(-tab) }
    )

    ptab <- base::prop.table(tab)
    names(tab)[is.na(names(tab))] <- "<NA>"

    z <- data.frame(class = names(tab),
    freq = as.vector(tab[]), perc = round(as.vector(ptab[]),digits),
    cumfreq = cumsum(tab[]), cumperc = round(cumsum(ptab[]),digits))

    rownames(z) <- NULL # enumerate from 1:nrow(z)
    class(z) <- c("freq", "data.frame")
    return(z)

  }
NULL
