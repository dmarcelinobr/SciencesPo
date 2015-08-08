#'  @title Load packages
#'
#' @description Check if package exists and install it otherwise.
#'
#' @param x The package name, or a list of packages.
#' @export
#' @examples
#' \dontrun{
#' package(qvalue) # from bioconductor
#' package(SoundexBR) # from CRAN
#' }
#'
package <- function(x) {
  x <- as.character(substitute(x))
  if(isTRUE(x %in% .packages(all.available=TRUE))) {
    eval(parse(text = sprintf("require(\"%s\")", x)))
  } else {
    eval(parse(text = sprintf("install.packages(\"%s\", dependencies = TRUE)", x)))
  }
  if(isTRUE(x %in% .packages(all.available=TRUE))) {
    eval(parse(text = sprintf("require(\"%s\")", x)))
  } else {
    source("http://bioconductor.org/biocLite.R")
    eval(parse(text = sprintf("biocLite(\"%s\")", x)))
    eval(parse(text = sprintf("require(\"%s\")", x)))
  }
}
