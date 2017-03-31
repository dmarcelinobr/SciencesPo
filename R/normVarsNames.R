#' @title Normalize Variable Names
#' @description (1) Replace any all capitals words to lowercase letter pattern.
#' (2) Remove any resulting initial or trailing underscore or multiples.
#' (3) Replace any number sequences not preceded by an
#  underscore, with it preceded by an underscore. (4) Remove repeated separators.
#'
#' @param x the data frame.
#' @param sep the separator character.
#'
#' @examples
#' names(iris) %<>% normalizeVariableNames() %>% print()
#'
#' @export
`normalizeVariableNames` <- function(x, sep="_")
{
  if (sep == ".") sep <- "\\."

  # Replace all _ and . and ' ' with the nominated separator. Note
  # that I used [] originally but that fails so use |.

  pat  <- '_|\u00a0|\u2022| |,|-|:|/|&|\\.|\\?|\\[|\\]|\\{|\\}|\\(|\\)'
  rep  <- sep
  x <- gsub(pat, rep, x)

  # Replace any all capitals words with Initial capitals. This uses an
  # extended perl regular expression. The ?<! is a zero-width negative
  # look-behind assertion that matches any occurrence of the following
  # pattern that does not follow a Unicode property (the \p) of a
  # letter (L) limited to uppercase (u). Not quite sure of the
  # use-case for the look-behind.

  pat  <- '(?<!\\p{Lu})(\\p{Lu})(\\p{Lu}*)'
  rep  <- '\\1\\L\\2'
  x <- gsub(pat, rep, x, perl=TRUE)

  # Replace any capitals not at the beginning of the string with _
  # and then the lowercase letter.

  pat  <- '(?<!^)(\\p{Lu})'
  rep  <- paste0(sep, '\\L\\1')
  x <- gsub(pat, rep, x, perl=TRUE)

  # WHY DO THIS? Replace any number sequences not preceded by an
  # underscore, with it preceded by an underscore. The (?<!...) is a
  # lookbehind operator.

  pat  <- paste0('(?<![', sep, '\\p{N}])(\\p{N}+)')
  rep  <- paste0(sep, '\\1')
  x <- gsub(pat, rep, x, perl=TRUE)

  # Remove any resulting initial or trailing underscore or multiples:
  #
  # _2level -> 2level

  x <- gsub("^_+", "", x)
  x <- gsub("_+$", "", x)
  x <- gsub("__+", "_", x)

  # Convert to lowercase

  x <- tolower(x)

  # Remove repeated separators.

  pat  <- paste0(sep, "+")
  rep  <- sep
  x <- gsub(pat, rep, x)

  return(x)
}
NULL
