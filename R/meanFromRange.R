#' @encoding UTF-8
#' @title  Estimates Mean and Standard Deviation from Median and Range

#' @description When conductig a meta-analysis study, it is not always possible to
#'  recover from reports, the mean and standard deviation values, but rather the
#'  median and range of values. This function provides a method to compute the
#'  median/range values into mean and variance estimates.
#'
#' @references
#' Hozo1, Stela P.; et al (2005) Estimating the mean and variance from the median, range, and the size of a sample. \emph{BMC Medical Research Methodology}, 5:13.
#'
#' @param low The min of the data.
#' @param med The median of the data.
#' @param high The max of the data
#' @param n The size of the sample.
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @export
#' @examples
#' MeanFromRange(5,8,12,10)
#'
`MeanFromRange` <-function(low, med, high, n) {
  mn<-(low+2*med+high)/4+(low-2*med+high)/(4*n)
  s=sqrt((low*low+med*med+high*high+(n-3)*((low+med)^2+(med+high)^2)/8-n*mn*mn)/(n-1))
  output <- data.frame('Mean'=mn, 'Variance'=s)
  class(output) <- c("SciencesPo", class(output))
  attr(output, "scpo.type") <- "Standard"
  return(output)
}
