#' @encoding UTF-8
#' @title Get Variable Label
#' @description This function returns character value previously stored in
#' variable's label attribute. If none found, and fallback argument is set to
#' \code{TRUE}, the function returns object's name (retrieved by
#' deparse(substitute(x))), otherwise \code{NA} is returned with a warning notice.
#' @param x an R object to extract labels from.
#' @param fallback a logical indicating if labels should fallback to object name(s). Default is set to \code{TRUE}.
#' @param simplify a logical indicating if coerce results to a vector,
#' otherwise, a list is returned. Default is set to \code{TRUE}.
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @rdname Label
#' @export
#' @examples
#'  x <- rnorm(100)
#' Label(x)             # returns "x"
#'
#' Label(twins$IQb) <- "IQ biological scores"
#'
#' Label(twins, FALSE)  # returns NA where no labels are found
#'
`Label` <- function (x, fallback = TRUE, simplify = TRUE)
{
  if (base::missing(x))
    stop("No variable provided.")
  if (is.null(x))
    return(NULL)
  if (is.atomic(x)) {
    lbl <- attr(x, which = "label", exact = TRUE)
    if (is.null(lbl)) {
      if (fallback) {
        lbl <- utils::tail(as.character(substitute(x)), 1)
      }
      else {
        warning("Atomic object has no labels.")
        lbl <- NA
      }
    }
    else {
      if (length(lbl) > 1) {
        warning("Variable label is not a length-one vector, only first element is returned.")
        lbl <- lbl[1]
      }
    }
  }
  else {
    lbl <- sapply(x, attr, which = "label", exact = TRUE)
    lbl.nil <- sapply(lbl, is.null)
    if (all(lbl.nil)) {
      if (fallback) {
        lbl <- structure(names(lbl), .Names = names(lbl))
      }
      else {
        warning("No labels found in recursive object,")
        lbl[lbl.nil] <- NA
      }
    }
    else {
      if (fallback)
        lbl[lbl.nil] <- names(lbl)[lbl.nil]
      else lbl[lbl.nil] <- NA
    }
  }
  if (simplify)
    lbl <- unlist(lbl)
  return(lbl)
}
NULL




#' @encoding UTF-8
#' @title Set Variable Label
#'
#' @description This function sets a label to a variable, by storing a character string to its \code{label} attribute.
#' @param x a valid variable i.e. a non-NULL atomic vector that has no dimension attribute (see dim for details).
#' @param value a character value that is to be set as variable label
#' @usage Label(x) <- value
#' @seealso \code{\link{label}}
#' @examples \dontrun{
#' Label(religiosity$Religiosity) <- "Religiosity index"
#'
#' vec <- rnorm(100)
#' Label(vec) <- "random numbers"
#' }
#' @export
#' @aliases Label<-
`Label<-` <- function(x, value){

  if (base::missing(x) | base::missing(value))
    stop('Both variable name and label should be provided.')

 if (!is.atomic(x) && is.null(x) && !is.null(dim(x)))
  stop('Label can only be assigned to a variable.')

  if (is.character(x) && length(x) == 1)
    stop('Only a character string can be assigned to a variable label.')

  attr(x, 'label') <- value

  return (x)
}
NULL





`Name` <- function(x){

  if (base::missing(x))
    stop('No variable provided')

  if (is.atomic(x)){
    n <- attr(x, which = 'name', exact = TRUE)
    if (is.null(n)) {
      return (tail(as.character(substitute(x)), 1)) # return variable name if no label
    } else {
      if (length(n) > 1)
        warning('variable name is not a length-one vector, only the first element is displayed')
      return(attr(x, 'name'))                       # return variable label
    }
  }

  if (is.recursive(x)){
    n <- sapply(x, attr, which = 'name', exact = TRUE)
    n.nil <- sapply(n, is.null)

    ## no labels found
    if (all(n.nil)){
      n <- names(n)
    } else
      n[n.nil] <- names(n)[n.nil]

    return(n)
  }

  stop('Wrong R object type provided!')
}
NULL


`Name<-` <- function(x, value){

  if (base::missing(x) | base::missing(value))
    stop('Both variable name and the new name should be provided.')

  if (!is.atomic(x) && is.null(x) && !is.null(dim(x)))
    stop('Name can only be assigned to a variable.')

  if (is.character(x) && length(x) == 1)
    stop('Only a character string can be assigned to a variable name')

  attr(x, 'name') <- value

  return (x)
}
NULL
