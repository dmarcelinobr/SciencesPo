# compatibility for data.table functions
## Look for existing generic functions also in imported namespaces.
## This will affect whether setGenericS3() creates a generic function
## or not.
options("R.methodsS3:checkImports:setGenericS3"=TRUE)

.datatable.aware <- TRUE
setnames <- `names<-`
setclass <- `class<-`

#' @title Chain operator
#' @name %>%
#' @importFrom magrittr %>%
#' @export %>%
#' @keywords manipulation
#' @rdname chain
#' @usage x %>% f(y) is translated into f(x, y).
NULL

#' Quote strings
#' @param \dots Any number of names separated by commas.
#' @export
#' @keywords manipulation
#' @examples
#' ssex[, qm(Date, Favor, DK)]
`qm` <- function(...)as.character(sys.call())[-1]
NULL

"%=%" <- function(x,y) {assign(as.character(substitute(x)), y, envir = parent.frame())}



`%overlaps%` <-
  function(x, y) {
    if(length(x) < 2) x <- rep(x, 2)
    if(length(y) < 2) y <- rep(y, 2)
    return(!(max(x) < min(y) | min(x) > max(y)) )
  }


`%like%` <-
  function(x, pattern) {

    if (!substr(pattern, 1, 1) == "%") {
      pattern <- paste("^", pattern, sep="")
    } else {
      pattern <- substr(pattern, 2, nchar(pattern) )
    }
    if (!substr(pattern, nchar(pattern), nchar(pattern)) == "%") {
      pattern <- paste(pattern, "$", sep="")
    } else {
      pattern <- substr(pattern, 1, nchar(pattern)-1 )
    }

    grepl(pattern = pattern, x = x)
  }



#' @title Find Matching (or Non-Matching) Elements
#' @description \code{\%nin\%} is a binary operator, which returns a logical vector indicating if there is a match or not for its left operand. A true vector element indicates no match in left operand, false indicates a match.
#' @param  x A vector (numeric, character, factor).
#' @param  y A vector (numeric, character, factor), matching the mode of \code{x}.
#' \code{\link{match}}, \code{\link{\%in\%}}.
#' @name %nin%
#' @rdname nin
#' @keywords Manipulation
#' @examples
#' c('a','b','c') %nin% c('a','b')
#' @export
`%nin%` <-
  function(x, y) match(x, y, nomatch = 0) == 0



#' @encoding UTF-8
#' @title Add quotation marks
#'@param vec the vector whose values will be surounded by quotes
#' @examples
#' x <- 1
#' quotize(x)
#' noquote(quotize(x))
#' a <- ("Daniel")
#' noquote(quotize(a))
#'
#'@export
`quotize` <- function(vec){
  sapply(vec, function(x) paste("'",x,"'",sep=''))}
NULL

#' @encoding UTF-8
#' @title Pause
#' @description A replication of MatLab pause function.
#' @param x is optional. If x>0 a call is made to \code{\link{Sys.sleep}}. Else, execution pauses until a key is entered.
#' @export
`pause` <-
  function (x=0) {
    if(x > 0){
      Sys.sleep(x)
    }else{
      cat("Hit <enter> to continue...","green")
      readline()
      invisible()
    }
  }
NULL



#' @param x A numeric vector.
#' @rdname .valid
#' @export
.valid<-function(x,na.rm=TRUE){
  return(ifelse(na.rm,sum(!is.na(x)),length(x)))}



is.formula <- function(expr) {
  inherits(expr, "formula") || (is.call(expr) && expr[[1L]] == "~")
}

dots <- function(...) {
  eval(substitute(alist(...)))
}

colNames <- function(data, cols) {
  if (!is.numeric(cols)) cols <- match(cols, names(data))
  names(data)[cols]
}
NULL

otherNames <- function(data, check) {
  setdiff(names(data), colNames(data, check))
}
NULL


user.prompt <- function (msg = NULL) {
  if (is.null(msg))
    msg <- "Press <return> to continue: "

  msg <- paste("\n", msg, sep="")

  invisible(readline(msg))
}
NULL



hour2min <- function(hhmm) {
  hhmm <- as.numeric(hhmm)
  trunc(hhmm/100)*60 + hhmm %% 100
}

min2hour <- function(min) {
  min <- as.numeric(min)
  trunc(min/60)*100 + min %% 60
}


#' @title Progress Bar
#' @param style An integer for style.
#' @param active A logical value.
#' @export
#' @importFrom utils txtProgressBar setTxtProgressBar
#'
.progress <- function(style = 3, active = TRUE, ...) {
  ntasks <- 0
  txt <- NULL
  max <- 0

  if (active) {
    list(
      init = function(x) {
        txt <<- txtProgressBar(max = x, style = style, ...)
        setTxtProgressBar(txt, 0)
        max <<- x
      },
      step = function() {
        ntasks <<- ntasks + 1
        setTxtProgressBar(txt, ntasks)
        if (ntasks == max) cat("\n")
      },
      term = function() close(txt)
    )
  } else {
    list(
      init = function(x) NULL,
      step = function() NULL,
      term = function() NULL
    )
  }
}
