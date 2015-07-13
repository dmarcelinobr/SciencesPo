##################################################################
## This will be sent to the console when the package is loaded.
##################################################################
.onLoad <- function(libname, pkgname) {
.scpo <<- new.env()
}

.onUnload <- function(libpath) {
  # Force finalize() on objects
  gc();
} # .onUnload()


 # No Visible Bindings
globalVariables(names=c("variable", "value", "num",
  ".data", ".temp", "v.tab", "Freq",
  "candidate.position", 'var.order', 'var.class',
  'var.size','var.lab', 'x_x', 'x_y'),
  package="SciencesPo", add=F)


