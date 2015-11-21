### =========================================================================
### The score() and `score<-`() generics
### -------------------------------------------------------------------------

setGeneric("score", function(x, ...) standardGeneric("score"))

setGeneric("score<-", signature="x",
           function(x, ..., value) standardGeneric("score<-")
)
NULL



### =========================================================================
### The strand() and `strand<-`() generics
### -------------------------------------------------------------------------

setGeneric("strand", function(x, ...) standardGeneric("strand"))

setGeneric("strand<-", function(x, ..., value) standardGeneric("strand<-"))

unstrand <- function(x)
{
  strand(x) <- "*"
  x
}






### =========================================================================
### The start(), `start<-`(), end(), `end<-`(), width(), and `width<-`()
### generics
### -------------------------------------------------------------------------
###
### stats::start and stats::end are S3 generics.

setGeneric("start")

setGeneric("start<-", signature="x",
           function(x, ..., value) standardGeneric("start<-")
)

setGeneric("end")

setGeneric("end<-", signature="x",
           function(x, ..., value) standardGeneric("end<-")
)

setGeneric("width", function(x) standardGeneric("width"))

setGeneric("width<-", signature="x",
           function(x, ..., value) standardGeneric("width<-")
)
