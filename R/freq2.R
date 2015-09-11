#' @title Frequency table
#' @description Computes absolute and relative frequencies of a vector x. Continuous (numeric) variables are cut as the \code{\link{hist}} function, whereas categorical variables are aggregated by \code{\link{table}}.
#'
#' @param x The variable to be described.
#' @param breaks Either a numeric vector of two or more cut points or a integer (> 2) for the intervals. If not provided, the default is taken from \code{hist()}. This parameter is ignored if x is not of class numeric.
#' @param digits An integer for the decimals.
#' @param include.lowest A logical, indicating if an x[i] equal to the lowest (or highest, for \code{right = FALSE}) \code{"breaks"} value should be included. Ignored if x is not of numeric type.
#' @param weighs a numeric vector of weights.
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
`freq2` <-
  function(x, weighs = NULL, breaks = hist(x, plot = FALSE)$breaks, digits=2, include.lowest = TRUE, ord = c("level", "desc", "asc", "name"),
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


#'  Recode very small levels into Missing values
#'
#'  The function recodes all levels with proportions below a certain threshold into "Missing".
#'  This is useful for a very crude first look at a lot of variables.
#'  @param variables is a data.frame of factors
#'  @param threshold is the proportion below which a level is recoded
#'  @param new.level is the new label for the recoded levels
#'  @return A data.frame
#'  @export
smallAsMissing <- function(variables, threshold=0.05, new.level="Missing"){
  for (i in 1:ncol(variables)){
    prop.var      <- prop.table(table(variables[,i]))
    kill.levels   <- names(prop.var)[prop.var < threshold]
    kl            <- levels(variables[,i]) %in% kill.levels
    levels(variables[,i])[kl]  <- new.level
  }
  variables
}
NULL

#' NA to Missing
#'
#' Recode all NA values into missing in a data.frame of factors
#'
#' @param x is a data.frame of factors
#' @param convert.to.factor if TRUE all non-factor columns in x are converted to factors
#' @param missing.value is the new value given to the NA values
#' @export
#' @examples
#'
#' x <- as.data.frame(matrix(rep(c("a", "b", "c", NA), 3), ncol=3))
#' x
#' NAasMissing(x)
NAasMissing <- function(x,convert.to.factor=FALSE, missing.value="MISSING"){

  # Handling if values are not factors
  if(identical(convert.to.factor, FALSE)){
    if (all(unlist(lapply(x, is.factor)))==FALSE) stop("Not all columns in x are factors")
  }
  if(identical(convert.to.factor, TRUE)){
    col.factors <- unlist(lapply(x, is.factor))
    if (all(col.factors)==FALSE){
      cat("Some columns where forced into factors")
      x[, col.factors] <- apply(x[,col.factors], 2, as.factor)
    }
  }
  # finding columns with NA values
  missing.cols  <- which(colSums(is.na(x)) > 0)
  missing.x     <- x[,missing.cols]

  for ( i in 1:ncol(missing.x)){
    levels(missing.x[,i])             <- c(levels(missing.x[,i]), missing.value)
    missing.x[is.na(missing.x[,i]),i] <- missing.value
  }
  x[,missing.cols]                    <- missing.x
  x
}
NULL
