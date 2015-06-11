#' @title Prompt for User Action
#'
#' @description Prompt user to hit enter
#' @param msg a character-string, specifying a message to be displayed
#' @return This function is used for its side effects
#' @export
#' @note This function is primarily used by SciencesPo scripts
user.prompt <- function (msg = NULL) {
  if (is.null(msg))
    msg <- "Press <return> to continue: "

  msg <- paste("\n", msg, sep="")

  invisible(readline(msg))
}

#' # make sure people are using a current version of R
r = R.Version()
if (r$major < "3" ||
    (r$major == "3" && r$minor < "2.0"))
{
  print(paste0(
    "The current version of R is 3.2.0, but you are using version ",
    r$major, ".", r$minor,
    ". Please google 'download R' and download the current version of R."
  ))
}

.initSciencesPo <- function() {
  if (exists(".temp",envir=.GlobalEnv)){
    assign(".temp",.temp,envir=.ScPoEnv)
    rm(.temp,envir=.GlobalEnv) } # Can no longer modify user's global environment
  if (!exists(".temp",envir=.ScPoEnv))
    assign(".temp",list(),envir=.ScPoEnv) #.GlobalEnv) #.PBSmod <<- list()
  tget(.temp)
  if (is.null(.temp$.options))
    #packList(".options",".temp",list()) #.temp$.options <<- list()
    .temp$.options <- list()
  if (is.null(.temp$.options$openfile))
    #packList("openfile",".temp$.options",list()) #.temp$.options$openfile <<- list()
    .temp$.options$openfile <- list()
  tput(.temp)
}

tget = function (x, penv=NULL, tenv=.ScPoEnv) {
  if (is.null(penv)) penv = parent.frame() # need to call this inside the function NOT as an argument
  xnam = as.character(substitute(x))
  if (exists(xnam,envir=tenv)) {
    eval(parse(text=paste("tgot=get(\"",xnam,"\",envir=tenv)",sep="")))
    eval(parse(text=paste("assign(\"",xnam,"\",tgot,envir=penv)",sep="")))
    return(invisible(tgot)) # useful for calling remote functions
  }
  invisible()
}

tput = function (x, penv=NULL, tenv=.ScPoEnv) {
  if (is.null(penv)) penv = parent.frame() # need to call this inside the function NOT as an argument
  xnam = as.character(substitute(x))
  if (exists(xnam,envir=penv))
    eval(parse(text=paste("assign(\"",xnam,"\",get(\"",xnam,"\",envir=penv),envir=tenv)",sep="")))
  invisible()
}

#' @title Detach All Data From the Memory
#'
#' @description Detach all data from the memory.
#'
#' @examples
#'detachAll()
#'
#' @export
detachAll <-
  function ()
  {
    pos.to.detach <- (1:length(search()))[substring(search(),
                                                    first = 1, last = 8) != "package:" & search() != ".GlobalEnv" &
                                            search() != "Autoloads" & search() != "CheckExEnv" & search() != "tools:rstudio" & search() != "TempEnv"]
    for (i in 1:length(pos.to.detach)) {
      if (length(pos.to.detach) > 0) {
        detach(pos = pos.to.detach[1])
        pos.to.detach <- (1:length(search()))[substring(search(),
                                                        first = 1, last = 8) != "package:" & search() !=".GlobalEnv" & search() != "Autoloads" & search() != "CheckExEnv" & search() != "tools:rstudio" & search() != "TempEnv"]
      }
    }
  }

# Zap
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


#' @title Column names as (always) a character vector
#'
#' @description A convenience function using either character vectors or numeric vectors to specify a subset of a \code{data.frame}.
#'
#' @param data the input \code{data.frame}.
#' @param cols the \code{names} or numeric position you want.
#' @return A character vector of the desired names.
#' @examples
#' \dontrun{col.names(iris, 1:3)}
#' @export
col.names <- function(data, cols) {
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
#'  @examples
#' \dontrun{ other.names(iris, "Species")}
#' @export
other.names <- function(data, check) {
  setdiff(names(data), col.names(data, check))
}
NULL


#' @title Converts calendar date string to POSIX
#'
#' @param x character vector in one of two calendar date formats
#' @return a POSIX date
#' @export
#' @author Daniel Marcelino \email{dmarcelino@@live.com}
posixify <- function(x) {
  x <- as.character(x)
  if(any(regexpr("^[[:digit:]]{1,2}/[[:digit:]]{1,2}/[[:digit:]]{4}$", x[1])[1] == 1))
    strptime(x, format="%m/%d/%Y") # short date format
  else
    strptime(x, format="%m/%d/%Y %I:%M:%S %p") # long date-time format
}


#' @title Method for building things
#' @param \dots some extra parameters.
#' @export
#' @docType methods
#' @rdname build-methods
#'
setGeneric("build", function(...){
  standardGeneric("build")
})

#' @title Method for latex
#' @param \dots some extra parameters.
#' @export
#' @docType methods
#' @rdname latex-methods
#'
setGeneric("latex", function(...){
  standardGeneric("latex")
})

#' @title Method for adding things.
#' @param \dots some extra parameters.
#' @export
#' @docType methods
#' @rdname add-methods
#'
setGeneric("add", function(...){
  standardGeneric("add")
})


#' @title Method for the number of observations
#'
#' @param object the data object
#' @param \dots some extra parameters.
#' @return The number of observations.
#'
#' @export
#' @docType methods
#' @rdname nobs-methods
#'
#' @examples
#' nobs(1:50)
#' nobs(10)
#'
setGeneric("nobs", function(object, ...){
  standardGeneric("nobs")
})

#' @rdname nobs-methods
#' @aliases nobs,numeric,ANY-method
setMethod("nobs", "numeric", function(object, ...){
  length(object)
})

#' @rdname nobs-methods
#' @aliases nobs,integer,ANY-method
setMethod("nobs", "integer", function(object, ...){
  length(object, ...)
})


#' @rdname nobs-methods
#' @aliases nobs,matrix,ANY-method
setMethod("nobs", "matrix", function(object, ...){
  NROW(object, ...)
})

#' @rdname nobs-methods
#' @aliases nobs,data.frame,ANY-method
setMethod("nobs", "data.frame", function(object, ...){
  NROW(object)
})



# compatibility for data.table functions
.datatable.aware <- TRUE

setnames <- `names<-`
setclass <- `class<-`

dots <- function(...) {
  eval(substitute(alist(...)))
}

ndots <- function(dots) {
  any(nzchar(dots))
}

is.formula <- function(expr) {
  inherits(expr, "formula") || (is.call(expr) && expr[[1L]] == "~")
}

is.side_effect <- function(expr) {
  is.formula(expr) &&
    (length(expr) == 2L ||
       length(expr) == 3L &&
       Recall(expr[[2L]]))
}



#' Helper function for determining the vector of attribute names
#' of a given object.
#'
#' @param obj [\code{mixed}]\cr
#'   Source object.
#' @return [\code{character}]
#'   Vector of attribute names for the source object.
#' @export
getAttrNames = function(obj) {
  if (!exists(".temp",envir=.ScPoEnv)) .initSciencesPo()
  return(names(attributes(obj)))
}


### short name wrapper functions
tab <- function(..., row.vars = NULL, col.vars = NULL, type = NULL,
	  style = "wide", decimals = 2, percent = TRUE, margins = TRUE,
	  subtotals = TRUE) {
	crosstab(..., row.vars = NULL, col.vars = NULL, type = NULL,
	  style = "wide", decimals = 2, percent = TRUE, margins = TRUE,
	  subtotals = TRUE)
  }
