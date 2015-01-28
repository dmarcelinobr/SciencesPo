

### Zap
zap <- 
  function () 
  {
    detachAll()
    vector1 <- setdiff(ls(envir = .GlobalEnv), lsf.str(envir = .GlobalEnv)[])
    rm(list = vector1, pos = 1)
  }

### List objects excluding function
lsNoFunction <- function() {
 setdiff(ls(envir= .GlobalEnv), as.character(lsf.str()[])
 )
}

### Limit maximum observations be print
print.data.frame <- function(x, ...) {
    oWidth <- getOption("width")
    oMaxPrint <- getOption("max.print")
    on.exit(options(width=oWidth, max.print=oMaxPrint))
    options(width=10000, max.print=300)
    base::print.data.frame(x, ...)
}

hour2min <- function(hhmm) {
  hhmm <- as.numeric(hhmm)
  trunc(hhmm/100)*60 + hhmm %% 100
}

min2hour <- function(min) {
  min <- as.numeric(min)
  trunc(min/60)*100 + min %% 60
}


#' Column names as (always) a character vector
#' 
#' A convenience function using either character vectors or numeric vectors to specify a subset of a \code{data.frame}.
#' 
#' 
#' @param data the input \code{data.frame}.
#' @param cols the \code{names} or numeric position you want.
#' @return A character vector of the desired names.
#' @examples
#' \dontrun{colNames(iris, 1:3)}
#' 
colNames <- function(data, cols) {
  if (!is.numeric(cols)) cols <- match(cols, names(data))
  names(data)[cols]
}
NULL



#' @encoding UTF-8
#' @title Extracts names from a dataset other than the ones indicates
#' 
#' @param data the input \code{data.frame}.
#' @param check The \code{names} you want to check.
#' 
#'  @return A character vector of the remaining names.
#'   @seealso \code{\link{setdiff}}
#'  @examples
#' \dontrun{ bigdf <- data.frame(a = 1:2, b = 3:4, c = 5:6)
#' otherNames(bigdf, "b")}
#'
otherNames <- function(data, check) {
  setdiff(names(data), colNames(data, check))
}
NULL
