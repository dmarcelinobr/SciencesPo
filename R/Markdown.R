`Markdown` <- function(style = "SciencesPo", ...) {
  # get the locations of resource files located within the package
  css = system.file("css", paste0(style,".css"), package = "SciencesPo")
  # call the base html_document function
  rmarkdown::html_document(css = css,
                           toc = TRUE,
                           toc_float = TRUE,
                           number_sections = TRUE,
                           code_folding = "hide",
                           highlight = "pygments",
                           theme = "default",
                           fig_width = 7,
                           fig_height = 4,
                           fig_align = "center",
                           ...)
}
NULL
