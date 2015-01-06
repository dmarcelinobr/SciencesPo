#' @title Subsets a data frame and drops the unused levels.
#'
#' @description Subsets a data frame and drops the unused levels.
#'
#' @details This function is used only for data frames.  It is equivalent to the combined usage of \code{subset} and \code{drop.levels}.  Use \code{subset} for all other structures.
#'
#' @note Newbie R users expect that when a factor variable is subsetted with \code{subset} that any original levels that are no longer used after the subsetting will be ignored. This, however, is not the case and often results in tables with empty cells and figures with empty bars. One remedy is to use \code{drop.levels} immediately following the \code{subset} call. Since this often becomes a repetitive sequence; thus, \code{Subset} method incorporates these two functions into one function.
#'
#' @note There is nearly no new code here. The code is from \code{subset} with a catch for non-data.frames and a specific call to \code{drop.levels} just before the data.frame is returned. I also added an argument to allow resetting the row names.
#'
#' @aliases Subset Subset.data.frame
#'
#' @param x A data frame.
#' @param subset A logical expression that indicates elements or rows to keep: missing values are taken as false.
#' @param select An expression, that indicates columns to select from a data frame.
#' @param drop passed on to \code{[} indexing operator.
#' @param resetRownames A logical that indicates if the rownames should be reset after the subsetting (\code{TRUE}; default).  Resetting rownames will simply number the rows from 1 to the number of rows in the result.

#' @param \dots further arguments to be passed to or from other methods.
#'
#' @return A data frame with the subsetted rows and selected variables.
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#'
#' @seealso \code{subset}, \code{drop.levels} in \pkg{gdata}, and \code{dropUnusedLevels} in \pkg{Hmisc}.
#' @importFrom gdata drop.levels 
#' 
#' @keywords Misc
#'
#' @examples
#' ## The problem -- note use of unused level in the final table.
#' levels(iris$Species)
#' iris.set1 <- subset(iris,Species=="setosa" | Species=="versicolor")
#' levels(iris.set1$Species)
#' table(iris.set1$Species)
#'
#' ## A simpler fix using Subset
#' iris.set2 <- Subset(iris,Species=="setosa" | Species=="versicolor")
#' levels(iris.set2$Species)
#' table(iris.set2$Species)
#'
#' @rdname Subset
#' @export
Subset <- function (x,...) {
  if (!is.data.frame(x)) stop("Subset should only be used with data frames.  For different data structure see ?subset.",call.=FALSE)
  UseMethod("Subset") 
}

#' @rdname Subset
#' @export
Subset.data.frame <- function (x,subset,select,drop=FALSE,resetRownames=TRUE,...) {
  if (missing(subset)) r <- TRUE
  else {
    e <- substitute(subset)
    r <- eval(e, x, parent.frame())
    if (!is.logical(r)) stop("'subset' must evaluate to logical.",call.=FALSE)
    r <- r & !is.na(r)
  }
  if (missing(select)) vars <- TRUE
  else {
    nl <- as.list(1:ncol(x))
    names(nl) <- names(x)
    vars <- eval(substitute(select),nl,parent.frame())
  }
  ans <- drop.levels(x[r,vars,drop = drop],reorder=FALSE)
  if (resetRownames) rownames(ans) <- NULL
  if (nrow(ans)==0) warning("The resultant data.frame has 0 rows. Try str() on the result.\n",call.=FALSE)
  ans
}
