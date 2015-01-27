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



.SciencesPoEnv <- new.env(parent=emptyenv()) # not exported

assign("SciencesPo.theme",   list(), envir = .SciencesPoEnv)
assign("SciencesPo.options", list(), envir = .SciencesPoEnv)

isLoaded <- function(.data) {
  exists(.data, .SciencesPoEnv)
}
