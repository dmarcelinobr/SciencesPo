#' @title Prompt for User Action
#' 
#' @description Prompt user to hit enter
#' @param msg a character-string, specifying a message to be displayed
#' @return This function is used for its side effects
#' @export
#' @note This function is primarily used by SciencesPo scripts
userPrompt <- function (msg = NULL) {
  if (is.null(msg))
    msg <- "Press <return> to continue: "

  msg <- paste("\n", msg, sep="")

  invisible(readline(msg))
}
