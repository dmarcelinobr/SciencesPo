#' @title A Tool Set For Analyzing Political Behavior Data
#'
#' @description SciencesPo is a facility package for the political science crowd. It provides a collection of functions for ease of analyze and presentation of random and nonrandom data. Use \code{help("SciencesPo")} for list all functions.
#' @import parallel
#'
#' @references Marcelino, Daniel (2013). \emph{SciencesPo: A Tool Set for Analyzing Political Behaviour Data}. Available at SSRN: \url{http://dx.doi.org/10.2139/ssrn.2320547}
#' @name SciencesPo
#' @docType package
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
NULL

#' @title Galton's Family Data on Human Stature.
#' 
#' @description It is a reproduction of the data set used by Galton in his 1885's paper on correlation between parent's height and their children. However, Galton would only introduce the concept of correlation few years later, in 1888. Galton suggested the use of the regression line and was the first to describe the so-called common phenomenon of regression toward the mean by comparing his experiments on the size of the seeds of successive generations of peas.
#'
#' \itemize{
#'   \item parent. The parents' average height
#'   \item child. The child's height
#' }
#'
#' @details Regression analysis is the statistical method most often used in political science research. The reason is that most scholars are interested in identifying \dQuote{causal} effects from non-experimental data and that regression is the method for doing this. The term \dQuote{regresssion} (1889) was first crafted by Sir Francis Galton upon investigating the relationship between body size of fathers and sons. Thereby he \dQuote{invented} regression analysis by estimating: \eqn{S_s = 85.7 + 0.56S_F} meaning that the size of the son regresses towards the mean.
#'
#' @references  Francis Galton (1886) Regression Towards Mediocrity in Hereditary Stature. \emph{The Journal of the Anthropological Institute of Great Britain and Ireland,} Vol. \bold{15}, pp. 246--263. 
#'
#' @docType data
#' @keywords datasets
#' @name galton
#' @usage data(galton)
#' @format A data frame with 928 observations on the following 2 variables.
NULL


#' @title Same Sex Marriage Public Opinion Data
#'
#' @description Data set fielded by the PEW Research Center on same sex marriage support in US. It covers public opinion on the issue starting from 1996 up to date.
#'
#' \itemize{
#'   \item Date The year of the measurement
#'   \item Oppose Percent opposing same-sex marriage
#'   \item Favor Percent favoring same-sex marriage
#'   \item DK Percent of Don't Know
#' }
#'
#' @references PEW Research Center. \emph{Support for same-sex marriage}. \url{http://www.pewresearch.org/data-trend/domestic-issues/attitudes-on-gay-marriage/}
#'
#' @docType data
#' @keywords datasets
#' @name ssex
#' @usage data(ssex)
#' @format A data frame with 18 observations on the following 4 variables.
NULL

