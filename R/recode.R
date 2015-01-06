#' Replace given values with new values, in a vector or factor

#'  Replace values with new values in a vector or factor. \emph{This is the same function as \code{mapvalues} in the \pkg{plyr} package}, though an enhancement was added; you can set a catch-all category using the \code{residual} option.

#' If \code{x} is a factor, the matching levels of the factor will be replaced with the new values.

#' I included it here, and renamed it to minimize conflicts between same-named functions in \pkg{dplyr} and \pkg{plyr}. The \pkg{car} package already has a function called \code{recode} that essentially does the same thing. It is meant to provide the untility of the RECODE statment of SPSS, however, I rather find its verbose a little fuzzy.
#'

#' @param x the factor or vector to modify

#' @param from a vector of the items to replace

#' @param to a vector of replacement values
#' @param residual a value to replace all the remaining values
#' @param warn print a message if any of the old values are not actually present in \code{x}

#'

#' @seealso See \code{mapvalues} in \pkg{plyr} for the sister function, or \code{revalue} in \pkg{plyr}; do the same thing but with a single named vector rather than on two separate vectors.

#'

#' @export

#'

#' @examples
#' #' # On numeric vectors
#' y <- sample(10)

#' recode(y, from = c(1, 5, 9), to = c(10, 50, 90))

#' # On factors

#' z <- factor(c("a", "b", "d", "c", "a"))

#' recode(z, c("a", "c"), c("A", "C"))

#' recode(z, from="d", to = "10")
#'
#'x = data.frame(var1 = c(0,0,1,1), var2 = c(0,1,0,1))
#'  x$new_vals <- recode(x$var1,from=c(0, 1), to = c(1, 0))
#'
recode <- function(x, from, to, residual = NULL, warn = TRUE) {
  if(!is.null(residual)){
    allrest <- setdiff(x, from)
    from <- c(from, allrest)
    to <- c(to, rep_len(residual, length(allrest)))
  }
  if (length(from) != length(to)) {
    stop("`from` and `to` vectors are not the same length.")
  }
  if (!is.atomic(x)) {
    stop("`x` must be an atomic vector.")
  }
  if (is.factor(x)) {
    # If x is a factor, call self but operate on the levels
    levels(x) <- recode(levels(x), from, to, residual, warn)
    return(x)
  }
  index_from <- match(x, from)
  index_NA  <- is.na(index_from)
  # index of items in `from` that were found in `x`
  from_found <- sort(unique(index_from))
  if (warn && length(from_found) != length(from)) {
    message("The following `from` values were not present in `x`: ",
            paste(from[!(1:length(from) %in% from_found) ], collapse = ", "))
  }
  x[!index_NA] <- to[ index_from[!index_NA]]
  x
}
