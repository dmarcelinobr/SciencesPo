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

RECYCLEWARNING <- NULL
.onLoad <- function(libname, pkgname){
  RECYCLEWARNING <<- gettext(tryCatch( (1:2)+(1:3),warning=function(w) w$message ))
}


print.data.frame <- function(x, ...) {
    oWidth <- getOption("width")
    oMaxPrint <- getOption("max.print")
    on.exit(options(width=oWidth, max.print=oMaxPrint))
    options(width=10000, max.print=300)
    base::print.data.frame(x, ...)
}

clc <- function() cat(rep("\n",50))