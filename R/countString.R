#' Count how often a pattern occurs in a character vector.
#'
#' @param x Character vector.
#' @param str Regular expression pattern passed to \code{gregexpr}.
#' @param ... Additional arguments passed to \code{\link{gregexpr}}.
#' @keywords character
#' @export
#' @examples
#' countString(c("doo", "dar", "daz"), "a");
#' countString(c("doo", "dar", "daz"), "^d")
countString <- function(x, str, ...) {
  vapply(gregexpr(str, x, ...), function(x) sum(x > 0L), 0, USE.NAMES=FALSE)
}
