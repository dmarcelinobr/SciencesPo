#' @encoding UTF-8
#' @title Deprecated functions in package \sQuote{SciencesPo}
#'
#' @description These functions are provided for compatibility with older versions
#' of \sQuote{SciencesPo} only, and will be defunct at the next release.
#' @name SciencesPo-deprecated
#' @param x A data.frame, a matrix-like, or a vector containing values for the number of votes or seats each party received.
#' @param index The type of index desired, one of "laakso/taagepera",  "golosov", "herfindahl", "gini", "shannon", "simpson", "invsimpson".
#' @param margin The margin for which the index is computed.
#' @param base The logarithm base used in some indices, such as the "shannon" index.
#' @param \dots Extra parameters.
#' @param y the dependent variable.
#'
#'
#' @details The following functions are deprecated and will be made defunct; use
#'  the replacement indicated below:
#'  \itemize{
#'      \item{svTransform: \code{\link{Normalize}}}
#'      \item{untable: \code{\link{Untable}}}
#'      \item{detail: \code{\link{Summary}}}
#'      \item{politicalDiversity: \code{\link{PoliticalDiversity}}}
#'      \item{winsorize: \code{\link{Winsorize}}}
#'      \item{recode: \code{\link{Recode}}}
#'      \item{dummy: \code{\link{Dummify}}}
#'      \item{gini: \code{\link{Gini}}}
#'      \item{clear: \code{\link{Clear}}}
#'      \item{destring: \code{\link{Destring}}}
#'      \item{outliers: \code{\link{Outlier}}}
#'      \item{voronoi: \code{\link{Voronoi}}}
#'      \item{jackknife: \code{\link{Jackknife}}}
#'      \item{bootstrap: \code{\link{Bootstrap}}}
#'      \item{normalize: \code{\link{Normalize}}}
#'
#'}
#'
#' @export
`svTransform` <- function(y)
{
  .Deprecated(Normalize, package="SciencesPo", "The `svTransform` method has been included in the more general `Normalize` function under the method 'SV'. Please, use `Normalize(..., method='SV')` instead.")
}
NULL

#' @rdname SciencesPo-deprecated
#' @export
`politicalDiversity` <- function(x, index = "laakso/taagepera", margin=1, base = exp(1))
{
  .Deprecated(PoliticalDiversity, package="SciencesPo", "The `politicalDiversity` method has been renamed with capital letter `PoliticalDiversity`. Please, use `PoliticalDiversity(...)` instead.")
}
NULL

#' @rdname SciencesPo-deprecated
#' @export
`untable`<- function(x, ...){
  .Deprecated(Untable, package="SciencesPo", "The `untable` method has been renamed with capital letter `Untable`. Please, use `Untable(...)` instead.")
}

# `svTransform <- function()
# {
#  .Defunct("Normalize")
# }`
