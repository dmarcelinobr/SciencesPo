tryCatch(utils::globalVariables(c('.SciencesPoEnv')),
         error=function(e) message('Looks like you should update R.'))

if((Rv <- getRversion()) < "3.1.0") {
  anyNA <- function(x) any(is.na(x))
  if(Rv < "3.0.0") {
    rep_len <- function(x, length.out) rep(x, length.out=length.out)
    if(Rv < "2.15"){
      paste0 <- function(...) paste(..., sep = '')
  }
}
}; rm(Rv)



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
#' @export
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
#'  @examples
#' \dontrun{ bigdf <- data.frame(a = 1:2, b = 3:4, c = 5:6)
#' otherNames(bigdf, "b")}
#' @export
otherNames <- function(data, check) {
  setdiff(names(data), colNames(data, check))
}
NULL


#' @title Gets Google Maps address
#'
#'
getGoogleMapsAddress <-  function(street = "Banacha 2", city = "Warszawa", country="Poland", positionOnly = TRUE, delay=1) {
  apiHttps  <- paste0("http://maps.googleapis.com/maps/api/geocode/json?address=", street, ",+",city,",+",country,"&sensor=true")
  res <- sapply(apiHttps, function(apiHttp) {
    Sys.sleep(delay)
    getGoogleMapsSignleAddress(apiHttp, positionOnly)
  })
  if (class(res) == "matrix") res <- t(res)
  res
}
NULL

#' @title Gets Google Maps single address
#'
#' @importFrom  rjson fromJSON
getGoogleMapsSignleAddress <-
  function(apiHttp, positionOnly = TRUE) {
    level <- 0
    apiHttp <- gsub(apiHttp, pattern=" ", replacement="\\+")
    jsnip <-fromJSON( file=apiHttp, method = "C" )

    if (length(jsnip[[1]]) == 0) {
      apiHttp <- gsub(apiHttp, pattern="[0-9]", replacement="")
      jsnip <- fromJSON( file=apiHttp, method = "C" )
      level <- 1
      if (length(jsnip[[1]]) == 0) {
        apiHttp <- gsub(apiHttp, pattern="address=[^,]*,", replacement="address=")
        jsnip <- fromJSON( file=apiHttp, method = "C" )
        level <- 2
        if (length(jsnip[[1]]) == 0) {
          apiHttp <- gsub(apiHttp, pattern="address=[^,]*,", replacement="address=")
          jsnip <- fromJSON( file=apiHttp, method = "C" )
          level <- 3
        }
      }
    }
    if (length(jsnip) == 2 & jsnip$status == "OVER_QUERY_LIMIT")
      return("OVER_QUERY_LIMIT")
    if (positionOnly)
      return(c(unlist(jsnip[[1]][[1]]$geometry$location),level))
    jsnip$level = level
    jsnip
  }
NULL


#' @title Convenience function to check color labels
#' @param  col the color name.
#'
pal <- function(col, border = "light gray")
{
  n <- length(col)
  plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1), axes=FALSE, xlab = "", ylab = "")
  rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
}


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


#' @title Method for build things
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
getAttributeNames = function(obj) {
  return(names(attributes(obj)))
}
