## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
library(SciencesPo)


## Do things 


detach("package:SciencesPo", unload=TRUE)


#You can also use the unloadNamespace command,

unloadNamespace("SciencesPo")


## ----echo=TRUE, message=FALSE--------------------------------------------
help.search('bar.plot')


help.search('twoway', package = 'SciencesPo')

## ----echo=TRUE, message=FALSE--------------------------------------------
vignette(package = "SciencesPo")

## ----echo=TRUE, message=FALSE--------------------------------------------
data(package = "SciencesPo")

## ----echo=FALSE, message=FALSE-------------------------------------------
require(SciencesPo)

set.seed(51)
 w <-sample(4,10, TRUE)
 x <- sample(10, 1000, replace=TRUE, prob=w)
 
skewness(x, type = 1);
kurtosis(x, type = 1);
skewness(x); # Type 2 is the default 
kurtosis(x); # Type 2 is the default 
skewness(x, type = 3);
kurtosis(x, type = 3);


## ----pres-ages, echo=TRUE, message=FALSE, cache=TRUE---------------------
pres =c(42,43,46,46,47,48,49,49,50,51,51,51,51,51,52,52,54,54,54,54,54,55,55,55,55,56,56,56,57,57,57,57,58,60,61,61,61,62,64,64,65,68,69)

ci(pres, level=.95) # confidence interval

ci(pres, level=.95)@mean # confidence interval

se(pres) # std. error

## ----echo=FALSE, message=FALSE-------------------------------------------
aad(pres) 


winsorize(pres)

## ----echo=FALSE, message=FALSE-------------------------------------------
require(SciencesPo)
str(iris)

iris_2 = safe.chars(iris)

str(iris_2)

## ----echo=TRUE, message=FALSE--------------------------------------------
require(SciencesPo)

mylevels <- c('Strongly Disagree', 
              'Disagree', 
              'Neither', 
              'Agree', 
              'Strongly Agree')

myvar <- factor(sample(mylevels[1:5], 10, replace=TRUE))

## ----echo=TRUE, message=FALSE--------------------------------------------
unclass(myvar) # testing the order

## ----echo=TRUE, message=FALSE--------------------------------------------
destring(myvar) 

## ----echo=TRUE, message=FALSE--------------------------------------------
 (x = seq(0, 1, by=.1))
 rounded(x) 

## ----one-way, eval=FALSE, echo=TRUE, message=FALSE, comment=NA-----------
#  CrossTabs(titanic$SURVIVED)

## ----Freq, echo=TRUE, message=FALSE, comment=NA--------------------------
Frequency(titanic, SURVIVED) 

## ----two-way, echo=TRUE, message=FALSE-----------------------------------
CrossTabs(titanic$SEX, titanic$SURVIVED) 

## ----politicalDiversity1, echo=TRUE, message=FALSE-----------------------
library("SciencesPo")

# The 1980 presidential election in the US (vote share):

(US1980 <- c("Democratic"=0.410, "Republican"=0.507,
              "Independent"=0.066, "Libertarian"=0.011,
              "Citizens"=0.003, "Others"=0.003));

politicalDiversity(US1980); # ENEP (laakso/taagepera) method 

politicalDiversity(US1980, index= "golosov");

politicalDiversity(US1980, index= "herfindahl");



## ----Helsinki-election, echo=TRUE, message=FALSE-------------------------
# Helsinki's 1999

Helsinki <- data.frame(votes = c(68885, 18343, 86448, 21982, 51587,
                                 27227, 8482, 7250, 365, 2734, 1925,
                                 475, 1693, 693, 308, 980, 560, 590, 185),
                       seats_SL=c(5, 1, 6, 1, 4, 2, 1, 0, 0, 0, 0, 0, 0, 0,
                                  0, 0, 0, 0, 0),
                       seats_DH=c(5, 1, 7, 1, 4, 2, 0, 0, 0, 0, 0, 0, 0, 0,
                                  0, 0, 0, 0, 0))

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
# politicalDiversity(Helsinki$votes); #ENEP Votes

politicalDiversity(Helsinki$seats_SL); #ENP for Saint-Lague

politicalDiversity(Helsinki$seats_DH); #ENP for D'Hondt

## ----Ceara-election, echo=TRUE, cache=TRUE-------------------------------
# Results for the state legislative house of Ceara (2014):
Ceara <- c("PCdoB"=187906, "PDT"=326841,"PEN"=132531, "PMDB"=981096,
           "PRB"=2043217,"PSB"=15061, "PSC"=103679, "PSTU"=109830,
           "PTdoB"=213988, "PTC"=67145, "PTN"=278267)

## ----highestAverages1, echo=TRUE, message=FALSE, comment=NA--------------
highestAverages(parties=names(Ceara), votes=Ceara,
                seats = 42, method = "dh") 

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
highestAverages(parties=names(Ceara), votes=Ceara,
                seats = 42, method = "sl") 

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
highestAverages(parties=names(Ceara), votes=Ceara, 
                seats = 42, method = "msl") 

