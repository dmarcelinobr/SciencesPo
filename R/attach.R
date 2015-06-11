##################################################################
## This will be sent to the console when the package is loaded.
##################################################################


.onLoad <- function(libname, pkgname)
{
#  library.dynam("SciencesPo", pkgname, libname)
}

.onAttach <- function(libname, pkgname) {
  .ScPoEnv <- new.env(FALSE, parent=globalenv())#Taking cue from Roger Bivand's maptools
  assign("SciencesPo.options", list(), envir = .ScPoEnv)
  .ScPoEnv = pos.to.env(match('package:SciencesPo', search()))
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


# No Visible Bindings
# ===================
if(getRversion() >= "2.15.1") utils::globalVariables(names=c(
  ".data", ".temp", "tgot", "v.tab", "Freq", "candidate.position", 'var.order', 'var.class','var.size','var.lab', 'x_x', 'x_y'), package="SciencesPo")



#.libName-------------------------------2009-07-08
#  Given a character vector of shared library object
#  names, returns the filenames with the appropriate
#  extension for the user's platform (.dll for Windows or .so
#  for Unix)
# Input:
#  lib - vector of filenames without extensions
# Output:
#   what the corresponding filenames should be on the current
#   platform
#-----------------------------------------------AE
.libName=function(lib=""){
  if (.Platform$OS.type=="windows")
    return(paste(lib, ".dll", sep=""))
  else
    return(paste(lib, ".so", sep=""))
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



changes <-
function(pkg="SciencesPo") {
    if(pkg=="SciencesPo") file.show(file.path(system.file(package="SciencesPo"), "NEWS"))
}

.onUnload <- function(libpath) {
  rm(.ScPoEnv)
}
#isLoaded <- function(.data) {
#  exists(.data, .ScPoEnv)
#}

#getData <- function(.data) {
#  if (!isLoaded(.data))
#    data(.data, envir =.ScPoEnv)
#  .ScPoEnv[[.data]]
#}
