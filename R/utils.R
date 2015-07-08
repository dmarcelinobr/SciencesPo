#.onAttach <- function(libname, pkgname) {
#  .ScPoEnv <- new.env(FALSE, parent=globalenv() )#Taking cue from Roger Bivand's maptools
#  assign("SciencesPo.options", list(), envir = .ScPoEnv)
#  .ScPoEnv = pos.to.env(match('package:SciencesPo', search()))
  ## Send message
#  msg <- paste("\n\n")
#  msg <- paste(msg,"                        000--------001\n")
#  msg <- paste(msg,"                          |\\       |\\\n")
#  msg <- paste(msg,"                          | \\      | \\\n")
#  msg <- paste(msg,"                          |100--------101\n")
#  msg <- paste(msg,"                        010--|- - -011|\n")
#  msg <- paste(msg,"                           \\ |      \\ |\n")
#  msg <- paste(msg,"                            \\|       \\|\n")
#  msg <- paste(msg,"                           110--------111\n")
#  packageStartupMessage(msg)
  #}
#.onUnload <- function(libpath) {
#  rm(.ScPoEnv)
# }

## The below .locale() is a local function
.locale <- local({
  val <- FALSE  # All automatic graphs will initially have English titles
  function(new){
    if(!missing(new))
      val <<- new
    else
      val
  }
})


`%c%` <- function(x, y) paste(x, y, sep="")


"%=%" <- function(x,y) {assign(as.character(substitute(x)), y, envir = parent.frame())}


`%nin%` <-
  function(x, table) match(x, table, nomatch = 0) == 0


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




#' @title Detach all data frame from the search path
#'
#' @description Detach all data frame from the search path, but keeping it on the memory.
#'
#' @examples
#' detach.all()
#'
#' @export
detach.all <- function ()
  {
    pos.to.detach <- (1:length(search()))[substring(search(),
                                                    first = 1, last = 8) != "package:" & search() != ".GlobalEnv" &
                                            search() != "Autoloads" & search() != "CheckExEnv" & search() != "tools:rstudio" & search() != "TempEnv"]
    for (i in 1:length(pos.to.detach)) {
      if (length(pos.to.detach) > 0) {
        detach(pos = pos.to.detach[1])
        pos.to.detach <- (1:length(search()))[substring(search(),
                                                        first = 1, last = 8) != "package:" & search() !=
                                                ".GlobalEnv" & search() != "Autoloads" & search() !=
                                                "CheckExEnv" & search() != "tools:rstudio" &
                                                search() != "TempEnv"]
      }
    }
  }




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
NULL



zap <-
  function ()
  {
    detach.all()
    vector1 <- setdiff(ls(envir = .GlobalEnv), lsf.str(envir = .GlobalEnv)[])
    rm(list = vector1, pos = 1)}

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
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @export
`pause` <-
  function (x=0) {
    if(x > 0){
      Sys.sleep(x)
    }else{
      cat("Hit <enter> to continue...")
      readline()
      invisible()
    }
  }
NULL

#' Check for differences in data.frames
#'
#' @param A The original data object.
#' @param B The other data.frame
#' @export
setdiff.data.frame = function(A, B){
  g <-  function( y, B){
    any( apply(B, 1, FUN = function(x)
      identical(all.equal(x, y), TRUE) ) ) }
  unique( A[ !apply(A, 1, FUN = function(t) g(t, B) ), ] )
}
NULL


packages<-function(x, repos="http://cran.r-project.org", ...){
  x <- deparse(substitute(x))
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x, repos=repos, ...)
    require(x,character.only=TRUE)
  }
}

rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}
rep.col<-function(x,n){
  matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}
NULL

