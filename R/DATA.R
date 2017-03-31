#' @encoding UTF-8
#' @title Galton's Family Data on Human Stature.
#'
#' @description It is a reproduction of the data set used by Galton in his 1885's paper on correlation between parent's height and their children, although the concept of correlation would be only introduced few years later, in 1888. This data.frame contains the following columns:
#'
#' \itemize{
#'   \item parent The parents' average height
#'   \item child The child's height
#' }
#'
#' @details Regression analysis is the statistical method most often used in political science research. The reason is that most scholars are interested in identifying \dQuote{causal} effects from non-experimental data and that the linear regression is the method for doing this.
#' The term \dQuote{regression} (1889) was first crafted by Sir Francis Galton upon investigating the relationship between body size of fathers and sons. Thereby he \dQuote{invented} regression analysis by estimating: \eqn{S_s = 85.7 + 0.56S_F}, meaning that the size of the son regresses towards the mean.
#'
#' @references Francis Galton (1886) Regression Towards Mediocrity in Hereditary Stature. \emph{The Journal of the Anthropological Institute of Great Britain and Ireland,} Vol. \bold{15}, pp. 246--263.
#'
#' @docType data
#' @keywords datasets
#' @name galton
#' @usage data(galton)
#' @format
#' A \code{data.frame} object with \Sexpr[stage=build,results=rd]{ncol(SciencesPo::galton)} variables and \Sexpr[stage=build,results=rd]{nrow(SciencesPo::galton)} observations.
NULL



#' @encoding UTF-8
#' @title Same Sex Marriage Public Opinion Data
#'
#' @description Data set fielded by the PEW Research Center on same sex marriage support in US. It covers public opinion on the issue starting from 1996 up to date. This dataset contains the following columns:
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
#' @name marriage
#' @usage data(marriage)
#' @format A \code{data.frame} object with \Sexpr[stage=build,results=rd]{ncol(SciencesPo::marriage)} variables and \Sexpr[stage=build,results=rd]{nrow(SciencesPo::marriage)} observations.
NULL




#' @encoding UTF-8
#' @title Titanic

#' @description Population at Risk and Death Rates for an Unusual Episode. For each person on board the fatal maiden voyage of the ocean liner Titanic, this dataset records sex, age [adult/child], economic status [first/second/third class, or crew] and whether or not that person survived. This dataset contains the following columns:
#'
#' \itemize{
#' \item CLASS Class (0 = crew, 1 = first, 2 = second, 3 = third)
#' \item AGE Age   (1 = adult, 0 = child)
#' \item SEX Sex   (1 = male, 0 = female)
#' \item SURVIVED Survived (1 = yes, 0 = no)
#' }
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
#' @format A \code{data.frame} object with \Sexpr[stage=build,results=rd]{ncol(SciencesPo::titanic)} variables and \Sexpr[stage=build,results=rd]{nrow(SciencesPo::titanic)} observations.
NULL






#' @encoding UTF-8
#' @title Turnout Data
#'
#' @description Data on voter turnout in the 50 states and D.C. for the 1992
#' Presidential election and 1990 Congressional elections. Per capita income,
#' populations in poverty and populations with no high school degree are also given. This dataset contains the following columns:
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
#' \item v11 South = 1; all others = 0.
#'  }
#'
#' @source U.S. Bureau of the Census, Statistical Abstract of the United States, 1994.
#'
#' @docType data
#' @keywords datasets
#' @name turnout
#' @usage data(turnout)
#' @format A \code{data.frame} object with \Sexpr[stage=build,results=rd]{ncol(SciencesPo::turnout)} variables and \Sexpr[stage=build,results=rd]{nrow(SciencesPo::turnout)} observations.
NULL





#' @encoding UTF-8
#' @title Paired Data
#'
#' @description Artificial data for a paired experiment. This dataset contains the following columns:
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
#' @format A \code{data.frame} object with \Sexpr[stage=build,results=rd]{ncol(SciencesPo::paired)} variables and \Sexpr[stage=build,results=rd]{nrow(SciencesPo::paired)} observations.
NULL






#' @encoding UTF-8
#' @title Word frequencies from Mosteller and Wallace
#'
#' @description The data give the frequencies of words in works from four different sources: the political writings of eighteenth century American political figures Alexander Hamilton, James Madison, and John Jay, and the book Ulysses by twentieth century Irish writer James Joyce. This dataset contains the following columns:
#'
#' \itemize{
#' \item Hamilton Hamilton frequency.
#' \item HamiltonRank Hamilton rank.
#' \item Madison Madison frequency.
#' \item MadisonRank Madison rank.
#' \item Jay Jay frequency.
#' \item JayRank Jay rank.
#' \item Ulysses Word frequency in Ulysses.
#' \item UlyssesRank Word rank in Ulysses.
#'  }
#'
#'  @references Weisberg, S. (2014). Applied Linear Regression, 4th edition. Hoboken NJ: Wiley.

#' @source Mosteller, F. and Wallace, D. (1964). Inference and Disputed Authorship: The Federalist. Reading, MA: Addison-Wesley.
#'
#' @docType data
#' @keywords datasets
#' @name words
#' @usage data(words)
#' @format A \code{data.frame} object with \Sexpr[stage=build,results=rd]{ncol(SciencesPo::words)} variables and \Sexpr[stage=build,results=rd]{nrow(SciencesPo::words)} observations.
NULL






#' @encoding UTF-8
#' @title Cathedrals
#'
#' @description Heights and lengths of Gothic and Romanesque cathedrals. This dataset contains the following columns:
#'
#' \itemize{
#' \item Type Romanesque or Gothic.
#' \item Height Total height, feet.
#' \item Length Total length, feet.
#'  }
#'
#'  @references
#' Weisberg, S. (2014). Applied Linear Regression, 4th edition. Hoboken NJ: Wiley.
#'
#' @docType data
#' @keywords datasets
#' @name cathedrals
#' @usage data(cathedrals)
#' @format A \code{data.frame} object with \Sexpr[stage=build,results=rd]{ncol(SciencesPo::cathedrals)} variables and \Sexpr[stage=build,results=rd]{nrow(SciencesPo::cathedrals)} observations.
NULL





#' @encoding UTF-8
#' @title Burt's Twin Data
#'
#' @description The given data are IQ scores from identical twins; one raised in a foster home, and the other raised by birth parents.
#' This dataset contains the following columns:
#'
#' \itemize{
#' \item C Social class, C1=high, C2=medium, C3=low, a factor.
#' \item IQb biological.
#' \item IQf foster.
#'  }
#'
#'  @references
#'  Weisberg, S. (2014). Applied Linear Regression, 4th edition. Hoboken NJ: Wiley.

#' @source Burt, C. (1966). The genetic estimation of differences in intelligence:
#'  A study of monozygotic twins reared together and apart. Br. J. Psych., 57, 147-153.
#'
#' @docType data
#' @keywords datasets
#' @name twins
#' @usage data(twins)
#' @format A \code{data.frame} object with \Sexpr[stage=build,results=rd]{ncol(SciencesPo::twins)} variables and \Sexpr[stage=build,results=rd]{nrow(SciencesPo::twins)} observations.
NULL






#' @encoding UTF-8
#' @title The Measure of US Presidents
#'
#' @description The US presidents and their main opponents' heights (cm). This dataset contains the following columns:
#'
#' \itemize{
#' \item election The election year.
#' \item winner The winner candidate.
#' \item winner.height The winner candidate's height in centimeters (cm).
#' \item winner.vote The popular vote support for the winner.
#' \item winner.party The winner's party.
#' \item opponent The main opponent candidate.
#' \item opponent.height The opponent candidate's height in centimeters (cm).
#' \item opponent.vote Popular vote support for the main opponent candidate.
#' \item opponent.party The opponent's party.
#' \item turnout The electorate turnout in percentages.
#' \item winner.bmi The winner Body Mass Index (BMI) estimate \code{(BMI = weight in kg/(height in meter)**2)}
#'  }
#'  @references
#' Murray, G. R. (2014) Evolutionary preferences for physical formidability in leaders. \emph{Politics and the Life Science}, 33(1), 33-53.
#'
#' @source
#' US Presidents: \url{http://www.lingerandlook.com/Names/Presidents.php}
#' Inside Gov. \url{http://www.us-presidents.insidegov.com}.
#'
#' @docType data
#' @keywords datasets
#' @name stature
#' @usage data(stature)
#' # t.test(stature$winner.height, stature$opponent.height, var.equal=TRUE)
#' @format A \code{data.frame} object with \Sexpr[stage=build,results=rd]{ncol(SciencesPo::stature)} variables and \Sexpr[stage=build,results=rd]{nrow(SciencesPo::stature)} observations.
NULL






#' @encoding UTF-8
#' @title Approval Ratings for President George W. Bush
#'
#' @description Approval ratings for George W. Bush.
#'
#' @format A \code{data.frame} object with \Sexpr[stage=build,results=rd]{ncol(SciencesPo::bush)} variables and \Sexpr[stage=build,results=rd]{nrow(SciencesPo::bush)} observations.
#' \itemize{
#' \item start.date. Start date of the survey.
#' \item end.date. End date of the survey.
#' \item approve. Percent which approve of the president.
#' \item disapprove. Percent which disapprove of the president.
#' \item undecided. Percent undecided about the president.
#' }
#' @details A data set of approval ratings of George Bush over the time of
#'  his presidency, as reported by several agencies. Most polls were
#'  of size approximately 1,000 so the margin of error is about 3 percentage points.
#'  @source
#'  \url{http://www.pollingreport.com/BushJob.htm}
#'
#' @docType data
#' @keywords datasets
#' @name bush
#' @usage data(bush)
#'
NULL





#' @encoding UTF-8
#' @title Polls for 2008 U.S. presidential election
#'
#' @description Polls for the 2008 U.S. presidential election.
#' The data includes all presidential polls reported on the
#' internet site \url{http://www.elections.huffingtonpost.com/pollster} that were taken
#' between August 29th, when John Mc-Cain announced
#' that Sarah Palin would be his running mate as the Republican
#' nominee for vice president, and the end of September.
#'
#' @format A \code{data.frame} object with \Sexpr[stage=build,results=rd]{ncol(SciencesPo::pollster2008)} variables and \Sexpr[stage=build,results=rd]{nrow(SciencesPo::pollster2008)} observations.
#' \itemize{
#' \item PollTaker Polling organization.
#' \item PollDates Dates the poll data were collected.
#' \item MidDate Midpoint of the polling period.
#' \item Days Number of days after August 28th (end of Democratic convention).
#' \item n Sample size for the poll.
#' \item Pop \code{A}=all, \code{LV}=likely voters, \code{RV}=registered voters.
#' \item McCain Percent supporting John McCain.
#' \item Obama Percent supporting Barak Obama.
#' \item Margin Obama percent minus McCain percent.
#' \item Charlie Indicator for polls after Charlie Gibson interview with VP candidate Sarah Palin (9/11).
#' \item Meltdown Indicator for polls after Lehman Brothers bankruptcy (9/15).
#' }
#'  @source
#'  \url{http://www.elections.huffingtonpost.com/pollster}
#'
#' @docType data
#' @keywords datasets
#' @name pollster2008
#' @usage data(pollster2008)
#'
NULL





#' @encoding UTF-8
#' @title 2008 U.S. presidential election
#'
#' @description State-by-state information from the 2008 U.S. presidential election.
#'
#' @format A \code{data.frame} object with \Sexpr[stage=build,results=rd]{ncol(SciencesPo::election2008)} variables and \Sexpr[stage=build,results=rd]{nrow(SciencesPo::election2008)} observations.
#' \itemize{
#' \item State Name of the state.
#' \item Abr Abbreviation for the state.
#' \item Income Per capita income in the state as of 2007 (in dollars).
#' \item HS Percentage of adults with at least a high school education.
#' \item BA Percentage of adults with at least a college education.
#' \item Dem.Rep Difference in \%Democrat-\%Republican (according to 2008 Gallup survey).
#' \item ObamaWin \code{1}= Obama (Democrat) wins state in 2008 or \code{0}=McCain (Republican wins).
#' }
#'  @source
#' State income data from: Census Bureau Table 659. Personal Income Per Capita (in 2007).
#' High school data from: U.S. Census Bureau, 1990 Census of Population,
#' \url{http://www.nces.ed.gov/programs/digest/d08/tables/dt08_011.asp}.
#' College data from: Census Bureau Table 225. Educational Attainment by State (in 2007)
#'  Democrat and Republican data from:
#' \url{http://www.gallup.com/poll/114016/state-states-political-party-affiliation.aspx}
#' @docType data
#' @keywords datasets
#' @name election2008
#' @usage data(election2008)
#'
NULL




#' @encoding UTF-8
#' @title Data on religiosity of countries
#'
#' @description Data from the 2007 Spring Survey conducted through the Pew Global Attitudes Project.
#'
#' @format A \code{data.frame} object with \Sexpr[stage=build,results=rd]{ncol(SciencesPo::religiosity)} variables and \Sexpr[stage=build,results=rd]{nrow(SciencesPo::religiosity)} observations.
#' \itemize{
#' \item Country Name of country.
#' \item Religiosity A measure of degree of religiosity for residents of the country.
#' \item GDP Per capita Gross Domestic Product in the country.
#' \item Africa Indicator for countries in Africa.
#' \item EastEurope Indicator for countries in Eastern Europe.
#' \item MiddleEast Indicator for countries in the Middle East.
#' \item Asia Indicator for countries in Asia.
#' \item WestEurope Indicator for countries in Western Europe.
#' \item Americas Indicator for countries in North/South America.
#' }
#'  @source
#'  The Pew Research Center's Global Attitudes Project surveyed
#'  people around the world and asked (among many other questions) whether they agreed that "belief
#'  in God is necessary for morality," whether religion is very important in their lives, and
#'  whether the at least once per day. The variable Religiosity is the sum of the percentage
#'  of positive responses on these three items, measured in each of 44 countries. The dataset also
#'   includes the per capita GDP for each country and indicator variables that record the part of the world the country is in.
#'  \url{http://www.pewglobal.org}
#'
#' @docType data
#' @keywords datasets
#' @name religiosity
#' @usage data(religiosity)
NULL





#' @encoding UTF-8
#' @title Poll attitudes towards British trade unions
#'
#' @description The British polling company Ipsos MORI conducted several
#' opinion polls in the UK between 1975 and 1995 in which they asked whether
#' people agree or disagree with the statement "Trade unions have too much
#' power in Britain today".
#'
#' @format A \code{data.frame} object with \Sexpr[stage=build,results=rd]{ncol(SciencesPo::unions)} variables and \Sexpr[stage=build,results=rd]{nrow(SciencesPo::unions)} observations.
#' \itemize{
#' \item Date Month of the poll \code{Aug-77} to \code{Sep-79}.
#' \item AgreePct Percent who agree (unions have too much power).
#' \item DisagreePct Percent who disagree.
#' \item NetSupport DisagreePct-AgreePct.
#' \item Months Months since August 1975.
#' \item Late \code{1}=after 1986 or \code{0}=before 1986.
#' \item Unemployment Unemployment rate.
#' }
#'  @source
#'  \url{http://www.ipsos-mori.com/researchpublications/researcharchive/poll.aspx?oItemID=94}
#'
#' @docType data
#' @keywords datasets
#' @name unions
#' @usage data(unions)
#'
NULL



#' @encoding UTF-8
#' @title Voting correlations
#' @description A dataset with voting correlations of traits, where each
#' trait is measured for 17 developed countries (Europe, US,
#' Japan, Australia, New Zealand).
#'
#' @docType data
#' @keywords datasets
#' @name vote
#' @usage data(vote)
#' @format A \code{12x12 matrix} object with \Sexpr[stage=build,results=rd]{ncol(SciencesPo::vote)} variables and \Sexpr[stage=build,results=rd]{nrow(SciencesPo::vote)} observations.
#'
#' @source
#' Torben Iversen and David Soskice (2006). Electoral institutions and
#' the politics of coalitions: Why some democracies redistribute more than
#' others. \emph{American Political Science Review}, 100, 165-81. Table A2.
#'
#' @references
#' Using Graphs Instead of Tables.
#' \url{http://www.tables2graphs.com/doku.php?id=03_descriptive_statistics}
#'
NULL







#' @encoding UTF-8
#' @title A dataset that contains four test items
#'
#' @description A dataset with four test items used in SPSS to to compute Cronbach's alpha.
#'
#' \itemize{
#' \item q1:q4 Numeric items of a scale.
#' }
#' @docType data
#' @keywords datasets
#' @name alpha
#' @usage data(alpha)
#' @format A \code{data.frame} object with \Sexpr[stage=build,results=rd]{ncol(SciencesPo::alpha)} variables and \Sexpr[stage=build,results=rd]{nrow(SciencesPo::alpha)} observations.
#'
NULL


#' @encoding UTF-8
#' @title Bradley Efron and Carl Morris ("Stein's Paradox in Statistics)
#'
#' @description A dataset used in Efron and Morris's 1977 paper, "Stein's Paradox in Statistics".
#'
#' \itemize{
#' \item {name}{First and last name of selected player}.
#' \item {atBats}{number of times at bats}.
#' \item {hits}{number of hits after 45 at bats}.
#' \item {avg45}{is the average after 45 at bats}.
#' \item {remainingAtBats}{remaining number at bats}.
#' \item {remainingAvg}{is the remaining average to the end of the season}.
#' \item {seasonAtBats}{is the number of times at bats in the end of the season}.
#' \item {seasonHits}{is the number of hits in the end of the season}.
#' \item {avgSeason}{is the end of the season average}.
#' }
#' @docType data
#' @keywords datasets
#' @name baseball
#' @usage data(baseball)
#' @format A \code{data.frame} object with \Sexpr[stage=build,results=rd]{ncol(SciencesPo::baseball)} variables and \Sexpr[stage=build,results=rd]{nrow(SciencesPo::baseball)} observations.
#'
NULL



#' @encoding UTF-8
#' @title Statistics of 1979 automobile models
#'
#' @description The data give the following statistics for 74 automobiles in the 1979 model year as sold in the US.
#'
#' @docType data
#' @keywords datasets
#' @name auto
#' @usage data(auto)
#' @format A \code{data.frame} object with \Sexpr[stage=build,results=rd]{ncol(SciencesPo::auto)} variables and \Sexpr[stage=build,results=rd]{nrow(SciencesPo::auto)} observations.

#' \describe{
#' \item{\code{Model}}{Make and model of car.}
#' \item{\code{Origin}}{a factor with levels A,E,J}
#' \item{\code{Price}}{Price in dollars.}
#' \item{\code{MPG}}{Miles per gallon.}
#' \item{\code{Rep78}}{Repair record for 1978 on 1 (worst) to 5 (best) scale.}
#' \item{\code{Rep77}}{Repair record for 1978 on 1 to 5 scale.}
#' \item{\code{Hroom}}{Headroom in inches.}
#' \item{\code{Rseat}}{Rear seat clearance in inches.}
#' \item{\code{Trunk}}{Trunk volume in cubic feet.}
#' \item{\code{Weight}}{Weight in pounds.}
#' \item{\code{Length}}{Length in inches.}
#' \item{\code{Turn}}{Turning diameter in feet.}
#' \item{\code{Displa}}{Engine displacement in cubic inches.}
#' \item{\code{Gratio}}{Gear ratio for high gear.}
#' }
#'
#' @source This data frame was created from
#' \url{http://euclid.psych.yorku.ca/ftp/sas/sssg/data/auto.sas}.
#'
#' @references Originally published in Chambers, Cleveland, Kleiner, and Tukey,
#' \emph{Graphical Methods for Data Analysis}, 1983, pages 352-355.
#'
NULL



#' @encoding UTF-8
#' @title Smoking and Lung Cancer in China
#'
#' @description The Chinese smoking and lung cancer data (Agresti,  Table 5.12, p. 60).
#' \itemize{
#' \item {City}{city name}.
#' \item {Smoker}{if smoker or not}.
#' \item {Cancer}{if detected cancer or not}.
#' \item {Count}{the frequencies}.
#' }
#'  @references
#' Agresti, A. \emph{An introduction to categorical Data Analysis}, (2nd ed.). Wiley.
#'
#' @docType data
#' @keywords datasets
#' @name chismoke
#' @usage data(chismoke)
#' @format A \code{data.frame} object with \Sexpr[stage=build,results=rd]{ncol(SciencesPo::chismoke)} variables and \Sexpr[stage=build,results=rd]{nrow(SciencesPo::chismoke)} observations.
#'
NULL


#' @encoding UTF-8
#' @title Bollen Data on Industrialization and Political Democracy
#'
#' @description The data were reported in Bollen
#' (1989, p. 428, Table 9.4) This set includes data from
#' 75 developing countries each assessed on four measures
#' of democracy measured twice (1960 and 1965), and three
#' measures of industrialization measured once (1960).
#' \itemize{
#'\item{y1}{Freedom of the press, 1960}
#'\item{y2}{Freedom of political opposition, 1960}
#'\item{y3}{Fairness of elections, 1960}
#'\item{y4}{Effectiveness of elected legislature, 1960}
#'\item{y5}{Freedom of the press, 1965}
#'\item{y6}{Freedom of political opposition, 1965}
#'\item{y7}{Fairness of elections, 1965}
#'\item{y8}{Effectiveness of elected legislature, 1965}
#'\item{x1}{GNP per capita, 1960}
#'\item{x2}{Energy consumption per capita, 1960}
#'\item{x3}{Percentage of labor force in industry, 1960}
#'}

#'  @references
#'  Bollen, K. A. (1979). Political democracy and the timing of
#'  development. \emph{American Sociological Review}, 44, 572-587.
#'
#'  Bollen, K. A. (1980). Issues in the comparative measurement of
#' political democracy. \emph{American Sociological Review}, 45, 370-390.
#'
#' @docType data
#' @keywords datasets
#' @name bollen
#' @usage data(bollen)
#' @format A \code{data.frame} object with \Sexpr[stage=build,results=rd]{ncol(SciencesPo::bollen)} variables and \Sexpr[stage=build,results=rd]{nrow(SciencesPo::bollen)} observations.
#'
NULL


#' @encoding UTF-8
#' @title Big 5 dataset
#'
#' @description This is a dataset of the Big 5 personality traits. It is a measurement of the Dutch translation of the NEO-PI-R on 500 first year psychology students (Dolan, Oort, Stoel, Wicherts, 2009). The test consists of fifty items that you must rate on how true they are about you on a five point scale where 1=Disagree, 3=Neutral and 5=Agree. It takes most people 3-8 minutes to complete.
#'
#' \itemize{
#'\item{Neuroticism}{The level of neuroticism}
#'\item{Extraversion}{The level of extraversion}
#'\item{Openness}{The level of openness}
#'\item{Agreeableness}{The level of agreeableness}
#'\item{Conscientiousness}{The level of conscientiousness}
#'}

#' @references
#' Hoekstra, H. A., Ormel, J., & De Fruyt, F. (2003). NEO-PI-R/NEO-FFI:
#' Big 5 persoonlijkheidsvragenlijst. Handleiding \emph{Manual of the
#' Dutch version of the NEO-PI-R/NEO-FFI}. Lisse, The Netherlands:
#' Swets and Zeitlinger.
#'
#' Dolan, C. V., Oort, F. J., Stoel, R. D., & Wicherts, J. M. (2009).
#' Testing measurement invariance in the target rotates multigroup
#' exploratory factor model. \emph{Structural Equation Modeling},
#' 16, 295<96>314.

#' @docType data
#' @keywords datasets
#' @name big5
#' @usage data(big5)
#' @format A \code{data.frame} object with \Sexpr[stage=build,results=rd]{ncol(SciencesPo::big5)} variables and \Sexpr[stage=build,results=rd]{nrow(SciencesPo::big5)} observations.
#'
NULL




#' Haberman's Abortion, Education, and Religion Data
#'
#' A multi-way contingency table with the results of a survey concerning attitudes concerning abortion.
#'
#' @name abortion
#' @docType data
#' @keywords datasets
#' @usage data(abortion)
#' @format A 3x3x3 (contingency) table
#' @author 1972 National Opinion Research Center
#' @references Haberman, S. (1978). \emph{Analysis of Qualitative Data} 1, 2. Academic Press, Orlando, FL.
NULL

#' Politics by Personality
#'
#' A 2-by-2 contingency table comparing political identification and personality.
#'
#' @name politics
#' @docType data
#' @keywords datasets
#' @usage data(politics)
#' @format A 2x2 (contingency) table
#' @author David Kahle, simplifying the dataset found on p.622 of Sheskin (see References)
#' @references Sheskin, D. J. Handbook of Parametric and Nonparametric Statistical Procedures. 4ed. Chapman and Hall/CRC Press, 2007.
NULL


#' Haberman's Positive Margin no Three-way Interaction MLE
#'
#' Haberman's example of a log-linear model (the no-three way
#' interaction model) that lacks an MLE even though it has positive
#' margins.
#'
#' @name haberman
#' @docType data
#' @keywords datasets
#' @usage data(haberman)
#' @format A 2x2x2 (contingency) table
#' @author Haberman, S.
#' @references Haberman, S. (1974). \emph{The Analysis of Frequency
#'   Data} 1, 2. University of Chicago Press, Chicago, IL.
NULL



#' @encoding UTF-8
#' @title Age Distribution of Population in Brazil
#'
#' @description Age Distribution of Population in Brazil.
#' \itemize{
#' \item {Year}{year}
#' \item {AgeGroup}{the age groups}
#' \item {Thousands}{the counts}
#' }
#'
#' @docType data
#' @keywords datasets
#' @name brpopage
#' @usage data(brpopage)
#' @format A \code{data.frame} object with \Sexpr[stage=build,results=rd]{ncol(SciencesPo::brpopage)} variables and \Sexpr[stage=build,results=rd]{nrow(SciencesPo::brpopage)} observations.
#'
NULL


# @docType
# @format \Sexpr[stage=build,results=rd]{data(galton); SciencesPo:::describe_df(galton)}
##'
#' # setwd("~/SciencesPo/data")
#' # system("cp big5.txt big5-cp.txt")
#' # system("rm big5.txt.gz")
#' # system("gzip big5.txt")
#' # system("rm big5-cp.txt")

