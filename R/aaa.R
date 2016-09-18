.onAttach <- function(lib, pkg) {
  ver <- read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")
  packageStartupMessage(paste(pkg, ver))
  options("SciencesPo.options" = list(
    scipen = 10,
    quiet = TRUE))
  #ggplot2::theme_set(theme_pub())
}
NULL
# SciencesPo_env <- new.env()

