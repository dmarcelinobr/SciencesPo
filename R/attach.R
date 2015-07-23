################################################################
## This will be sent to the console when the package is loaded.
###############################################################
.ScPoEnv <- new.env(FALSE, parent=globalenv())  # Taking cue from Roger Bivand's maptools


.onUnload <- function(libpath) {
  rm(.ScPoEnv)
}


.onAttach <- function(libname, pkgname) {
  assign("SciencesPo.op", list(), envir = .ScPoEnv)
  pkgEnv = pos.to.env(match('package:SciencesPo', search()))
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
  suppressMessages(packageStartupMessage(msg))
}


 # No Visible Bindings
globalVariables(names=c("variable", "value", "num",
  ".data", ".temp", "v.tab", "Freq",
  "candidate.position", 'var.order', 'var.class',
  'var.size','var.lab', 'x_x', 'x_y'),
  package="SciencesPo", add=F)
