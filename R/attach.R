##################################################################
## This will be sent to the console when the package is loaded.
##################################################################
.onLoad <- function(libname, pkgname)
{
 # library.dynam("SciencesPo", pkgname, libname)
}


 # No Visible Bindings
globalVariables(names=c("variable", "value", "num",
  ".data", ".temp", "v.tab", "Freq",
  "candidate.position", 'var.order', 'var.class',
  'var.size','var.lab', 'x_x', 'x_y'),
  package="SciencesPo", add=F)


