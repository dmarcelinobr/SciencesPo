#' @title Examples from the SciencesPo Package
#'
#' @description Launch a Shiny app that shows a demo.
#' @param app the name of the shiny application.
#' @examples
#' # A demo of what \code{\link[SciencesPo]{PoliticalDiversity}} does.
#' if (interactive()) {
#' RunShinyApp(PoliticalDiversity)
#' }
#' @export
#'
`RunShinyApp` <- function(app){
# locate all the shiny app examples that exist
validExamples <- list.files(system.file("shiny-examples", package = "SciencesPo"))
validExamplesMsg <-
paste0(
"Valid examples are: '",
paste(validExamples, collapse = "', '"),
"'")
# if an invalid app is given, throw an error
 if (missing(app) || !nzchar(app) ||
   !app %in% validExamples) {
   stop(
'Please run `RunShinyApp()` with a valid example app as an argument.\n',
validExamplesMsg,
     call. = FALSE)
}
# find and launch the app
appDir <- system.file("shiny-examples", app, package = "SciencesPo")
shiny::runApp(appDir, display.mode = "normal")
}##--end of RunShinyApp
NULL
