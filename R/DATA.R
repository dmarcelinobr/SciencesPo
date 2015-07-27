
#' @title Galton's Family Data on Human Stature.
#'
#' @description It is a reproduction of the data set used by Galton in his 1885's paper on correlation between parent's height and their children. However, Galton would only introduce the concept of correlation few years later, in 1888. Galton suggested the use of the regression line and was the first to describe the so-called common phenomenon of regression toward the mean by comparing his experiments on the size of the seeds of successive generations of peas.
#'
#' \itemize{
#'   \item parent the parents' average height
#'   \item child the child's height
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
#' @references PEW Research Center. \emph{Support for same-sex marriage}. \url{http://www.pewresearch.org}
#'
#' @docType data
#' @keywords datasets
#' @name ssex
#' @usage data(ssex)
#' @format A data frame with 18 observations on the following 4 variables.
NULL


#' @title The Penn World Table
#'
#' @description The Penn World Table used in Summers and Heston (1991).
#'
#' \itemize{
#' \item year Year
#' \item pop Population (thousands)
#' \item rgdppc Real per capita GDP
#' \item savrat a numeric vector
#' \item country Country
#' \item com Communist regime
#' \item opec OPEC country
#' \item name Country name
#' }
#'
#' @references Summers, R. and Heston, A. (1991) The Penn World Table (Mark 5): an expanded set of international comparisons, 1950--1988. \emph{The Quarterly Journal of Economics,} \bold{106(2),} 327--368.
#'
#' @source Hayashi, F. (2000). \emph{Econometrics.} Princeton. New Jersey, USA: Princeton University. \url{http://fmwww.bc.edu/ec-p/data/Hayashi/}
#'
#'
#' @docType data
#' @keywords datasets
#' @name sheston91
#' @usage data(sheston91)
#' @format A data frame with 3250 observations on the following 8 variables.
NULL



#' @title Griliches's (1976) Data
#'
#' @description Data set used by Griliches (1976) on wages of very young men.
#'
#' \itemize{
#' \item rns residency in South.
#' \item rns80 a numeric vector.
#' \item mrt marital status = 1 if married.
#' \item mrt80 a numeric vector.
#' \item smsa reside metro area = 1 if urban.
#' \item smsa80 a numeric vector.
#' \item med mother's education, years.
#' \item iq iq score.
#' \item kww score on knowledge in world of work test.
#' \item year Year.
#' \item age a numeric vector.
#' \item age80 a numeric vector.
#' \item s completed years of schooling.
#' \item s80 a numeric vector.
#' \item expr experience, years.
#' \item expr80 a numeric vector.
#' \item tenure tenure, years.
#' \item tenure80 a numeric vector.
#' \item lw log wage.
#' \item lw80 a numeric vector.
#' }
#'
#' @source Hayashi, F. (2000). \emph{Econometrics.} Princeton. New Jersey, USA: Princeton University. \url{http://fmwww.bc.edu/ec-p/data/Hayashi/}
#'
#' @references Griliches, Z. (1976) Wages of very young men. \emph{The Journal of Political Economy,} \bold{84(4),} 69--85.
#'
#' @docType data
#' @keywords datasets
#' @name griliches76
#' @usage data(griliches76)
#' @format A data frame with 758 observations on the following 20 variables.
NULL



#' @title Marc Nerlove's (1963) data
#'
#' @description Data used by Marc Nerlove (1963) on returns of electricity supply.
#' \itemize{
#' \item totcost costs in 1970, MM USD.
#'  \item output output, billion KwH.
#' \item plabor price of labor.
#'  \item pfuel price of fuel.
#'  \item pkap price of capital.
#'  }
#'
#'
#'#' @source Hayashi, F. (2000). \emph{Econometrics.} Princeton. New Jersey, USA: Princeton University. \url{http://fmwww.bc.edu/ec-p/data/Hayashi/}
#'
#' @references Nerlove, M. (1963) Returns to Scale in Electricity Supply. In \emph{Measurement in Economics-Studies in Mathematical Economics and Econometrics in Memory of Yehuda Grunfeld,} edited by Carl F. Christ. Stanford: Stanford.
#'
#' @docType data
#' @keywords datasets
#' @name nerlove63
#' @usage data(nerlove63)
#' @format A data frame with 145 observations on the following 5 variables.
NULL


#' @title Stock's and Watson's (1993) Data.

#' @description Data set used by Stock and Watson (1993) to estimate co-integration.
#' \itemize{
#' \item lnm1 Log M1.
#' \item lnp Log NNP price deflator.
#' \item lnnnp Log NNP.
#' \item cprate A numeric vector.
#' \item year Commercial paper rate.
#'  }
#'
#' @source Hayashi, F. (2000). \emph{Econometrics.} Princeton. New Jersey, USA: Princeton University. \url{http://fmwww.bc.edu/ec-p/data/Hayashi/}
#'
#' @references Stock, J. H., and Watson, M. W. (1993) A simple estimator of cointegrating vectors in higher order integrated systems. \emph{Econometrica: Journal of the Econometric Society,} 783--820.
#'
#' @docType data
#' @keywords datasets
#' @name swatson93
#' @usage data(swatson93)
#' @format A data frame with 90 observations on the following 5 variables.
NULL




#' @title Lothian's and Taylor's (1996) Data Set

#' @description Data used by Lothian and Taylor (1996) on the real exchange rate behaviour.
#'
#' \itemize{
#' \item year Year
#' \item spot dollar/sterling exchange rate.
#' \item USwpi U.S. wholesale price index, 1914==100.
#' \item UKwpi U.K. wholesale price index, 1914==100.
#' }
#'
#' @source Hayashi, F. (2000). \emph{Econometrics.} Princeton. New Jersey, USA: Princeton University. \url{http://fmwww.bc.edu/ec-p/data/Hayashi/}
#'
#'
#' @references Lothian, J. R., and Taylor, M. P. (1996) Real exchange rate behavior: the recent float from the perspective of the past two centuries. \emph{Journal of Political Economy,} 488--509.
#'
#' @docType data
#' @keywords datasets
#' @name ltaylor96
#' @usage data(ltaylor96)
#' @format A data frame with 200 observations on the following 4 variables.
NULL



#' @title Christensen's and Greene's (1976) Data

#' @description Data set used by Christensen and Greene (1976) on economies of scale in US electric power generation.
#'
#' \itemize{
#' \item firmid Observation id.
#' \item costs Costs in 1970, MM USD.
#' \item output Output, million KwH.
#' \item plabor Price of labor.
#' \item pkap Price of capital.
#' \item pfuel Price of fuel.
#' \item labshr Labor's cost share.
#' \item kapshr Capital's cost share.
#' }
#'
#' @source Hayashi, F. (2000). \emph{Econometrics.} Princeton. New Jersey, USA: Princeton University. \url{http://fmwww.bc.edu/ec-p/data/Hayashi/}
#'
#' @references Christensen, L. R., and Greene, W. H. (1976) Economies of scale in US electric power generation. \emph{The Journal of Political Economy,} 655--676.
#'
#' @docType data
#' @keywords datasets
#' @name cgreene76
#' @usage data(cgreene76)
#' @format A data frame with 99 observations on the following 8 variables. See Hayashi (2000) for more details.
NULL



#' @title Mishkin's (1992) Data
#'
#' @description Data from the Frederic S. Mishkin (1992) paper \dQuote{Is the Fisher Effect for real?}.
#'
#' \itemize{
#'  \item year Year
#' \item mon a numeric vector
#' \item inf1mo a numeric vector
#' \item inf3mo a numeric vector
#' \item tbill1mo a numeric vector
#' \item tbill3mo a numeric vector
#' \item cpiu a numeric vector
#' \item quote a numeric vector
#' }
#'
#' @source Hayashi, F. (2000). \emph{Econometrics.} Princeton. New Jersey, USA: Princeton University. \url{http://fmwww.bc.edu/ec-p/data/Hayashi/}
#'
#' @references Mishkin, F. S. (1992) Is the Fisher effect for real?: A reexamination of the relationship between inflation and interest rates. \emph{Journal of Monetary Economics,} \bold{30(2),} 195--215.
#'
#' @docType data
#' @keywords datasets
#' @name mishkin92
#' @usage data(mishkin92)
#' @format A data frame with 491 observations on the following 8 variables.
NULL


#' @title Bekaert's and Hodrick's (1993) Data

#' @description Data set used by Bekaert and Hodrick (1993) on biases in the measurement of foreign exchange risk premiums.
#'
#' \itemize{
#'  \item date A character vector for date.
#'  \item jyspot Price of USD in JY, spot.
#'  \item jyfwd Price of USD in JY, 30-day forward.
#'  \item jys30 Price of USD in JY, spot market at 30-day forward deliver/date.
#'  \item dmspot Price of USD in DM, spot.
#'  \item dmfwd Price of USD in DM, 30-day forward
#'  \item dms30 Price of USD in DM, spot market at 30-day forward deliver/date.
#'  \item bpspot Price of USD in BP, spot.
#'  \item bpfwd Price of USD in BP, 30-day forward.
#'  \item bps30 Price of USD in BP, spot market at 30-day forward deliver/date.
#' \item quote A numeric vector.
#' }
#'
#' @source Hayashi, F. (2000). \emph{Econometrics.} Princeton. New Jersey, USA: Princeton University. \url{http://fmwww.bc.edu/ec-p/data/Hayashi/}
#'
#' @references Bekaert, G., and Hodrick, R. J. (1993) On biases in the measurement of foreign exchange risk premiums. \emph{Journal of International Money and Finance,} \bold{12(2),} 115-138.
#'
#' @docType data
#' @keywords datasets
#' @name bhodrick93
#' @usage data(bhodrick93)
#' @format A data frame with 778 observations on the following 11 variables. See Hayashi (2000) for details.
NULL


#' @title Titanic

#' @description Population at Risk and Death Rates for an Unusual Episode. For each person on board the fatal maiden voyage of the ocean liner Titanic, this dataset records sex, age [adult/child], economic status [first/second/third class, or crew] and whether or not that person survived.
#'
#'
#' \itemize{
#' \item CLASS Class (0 = crew, 1 = first, 2 = second, 3 = third)
#' \item AGE Age   (1 = adult, 0 = child)
#' \item SEX Sex   (1 = male, 0 = female)
#' \item SURVIVED Survived (1 = yes, 0 = no)
#'  }
#'
#' @note There is not complete agreement among primary sources as to the exact numbers on board, rescued, or lost. \bold{STORY BEHIND THE DATA:} The sinking of the Titanic is a famous event, and new books are still being published about it.  Many well-known facts--from the proportions of first-class passengers to the "women and children first" policy, and the fact that that policy was not entirely successful in saving the women and children in the third class--are reflected in the survival rates for various classes of passenger.  These data were originally collected by the British Board of Trade in their investigation of the sinking.
#'
#' @source British Board of Trade Inquiry Report (1990). \emph{Report on the Loss of the `Titanic' (S.S.)"}, Gloucester, UK: Allan Sutton Publishing.
#'
#' @references Dawson (1995). "The `Unusual Episode' Data Revisited" in the \emph{Journal of Statistics Education}.
#'
#' @docType data
#' @keywords datasets
#' @name titanic
#' @usage data(titanic)
#' @format  2201 observations, 4 variables.
NULL




#' @title NY Weather 1995-2014

#' @description Data represents average daily temperatures.
#'
#' \itemize{
#'  \item month the month recorded (starts on January, 1995)
#'   character vector for date.
#'  \item day the day of the month.
#'  \item year the year.
#'  \item temp the average daily temperatures ( \dQuote{24 hour} and \eqn{(min + max)/2}).
#'  }
#' @note In the original files, missing data is recorded as \dQuote{-99}, so must be replaced.
#'
#' @source  Archived by the National Climatic Data Center (NCDC) and downloaded at: \url{http://academic.udayton.edu/kissock/http/Weather/gsod95-current/NYNEWYOR.txt}
#'
#' @references Edward Tufte \emph{New York Weather Chart}, January 4, 2004, p. A-15.
#'
#' @docType data
#' @keywords datasets
#' @name weather
#' @usage data(weather)
#' @format A data frame with 7290 observations on the following 4 variables.
NULL

#' @title Turnout Data
#'
#' @description Data on voter turnout in the 50 states and D.C. for the 1992
#' Presidential election and 1990 Congressional elections. Per capita income,
#' populations in poverty and populations with no high school degree are also given.
#'
#' \itemize{
#' \item v1 state name (alphabetic, 20 characters)
#' \item v2 region of country:
#' 1 Northeast
#' 2 Midwest
#' 3 South
#' 4 West
#' \item v3 division within region:
#' 1 Northeast-New England
#' 2 Northeast-Middle Atlantic
#' 3 Midwest-East North Central
#' 4 Midwest-West North Central
#' 5 South-South Atlantic
#' 6 South-East South Central
#' 7 South-West South Central
#' 8 West-Mountain
#' 9 West-Pacific.
#' \item v4 "Elazar's state political culture assignments:
#' 1 = moralistic
#' 2 = individualistic
#' 3 = traditionalistic
#' \item v5 percent of population below poverty level, 1992.
#' \item v6 per capita personal income, 1993.
#' \item v7 percent casting votes for presidential electors, 1992.
#' \item v8 percent casting votes for U.S. Representatives, 1990.
#' \item v9 population without a high school degree, of those 25 years or older, 1990.
#' \item v10 population 25 years or older, 1990.
#' \item v11 South =1; all others =0.
#'  }
#'
#' @source U.S. Bureau of the Census, Statistical Abstract of the United States, 1994.
#'
#' @docType data
#' @keywords datasets
#' @name turnout
#' @usage data(turnout)
#' @format A data frame with 51 observations and 11 variables.
NULL



#' @title Paired Data
#'
#' @description Artificial experimental paired data
#'
#' \itemize{
#' \item patient the patient id.
#' \item before_X before treatment.
#' \item after_Y after treatment.
#'  }
#'
#' @docType data
#' @keywords datasets
#' @name paired
#' @usage data(paired)
#' @format A data frame with 9 observations on the following 3 variables.
NULL
