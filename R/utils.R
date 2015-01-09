##################################################################
## This will be sent to the console when the package is loaded.
##################################################################  
.onAttach <- function(libname, pkgname) {
   pkgEnv = pos.to.env(match('package:SciencesPo', search()))
   # vers <- read.dcf(system.file("DESCRIPTION",package=pkg,lib.loc=lib),fields="Version")
    ## Send message
    msg <- paste("\n\n")
    msg <- paste(msg,"                        000--------001\n")
    msg <- paste(msg,"                          |\\       |\\\n")
    msg <- paste(msg,"                          | \\      | \\\n")
    msg <- paste(msg,"                          |100--------101\n")
    msg <- paste(msg,"                        010--|- - -011|\n")
    msg <- paste(msg,"                           \\ |      \\ |\n")
    msg <- paste(msg,"                            \\|       \\|\n")
    msg <- paste(msg,"                           110--------111\n")
    packageStartupMessage(msg)
  }

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
.distribution.of <- "Distribution of"
.by <- "by"
.frequency <- "Frequency"
.frequency1 <- "Frequency"
.No.of.observations <- "# of Observations = "
.ylab.for.summ <- "Subject sorted by X-axis values"
.percent <- "Percent"
.cum.percent <- "Cum. percent"
.var.name <- "Var. name"
.obs <- "obs."
.mean <- "mean  "
.median <- "median "
.sd <- "s.d.  "
.min <- "min.  "
.max <- "max.  "


RECYCLEWARNING <- NULL
.onLoad <- function(libname, pkgname){
  RECYCLEWARNING <<- gettext(tryCatch( (1:2)+(1:3),warning=function(w) w$message ))
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

