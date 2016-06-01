## ----eval=FALSE, echo=TRUE, message=FALSE, comment=NA--------------------
#  install.packages("SciencesPo", dependencies = c("Depends", "Suggests"))

## ----eval=FALSE, echo=TRUE, message=FALSE, comment=NA--------------------
#  library('SciencesPo')
#  
#  ## Do things ...
#  
#  detach('package:SciencesPo', unload = TRUE)

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
library("SciencesPo")

vignette(package = "SciencesPo")

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
data(package = "SciencesPo")

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
help.search("D'Hondt", package = 'SciencesPo')

## ----eval=FALSE, echo=TRUE, message=FALSE, comment=NA--------------------
#  RSiteSearch("D'Hondt")

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
data("titanic")
Describe(titanic) 

## ----eval=FALSE, echo=TRUE, message=FALSE, comment=NA--------------------
#  with(titanic, Crosstable(SURVIVED))

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
Freq(titanic, SURVIVED) 

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
with(titanic, Crosstable(SEX, CLASS))

## ----eval=FALSE, echo=TRUE, message=FALSE, comment=NA--------------------
#  Crosstable(titanic, SEX, CLASS, column = FALSE)

## ----eval=FALSE, echo=TRUE, message=FALSE, comment=NA--------------------
#  Crosstable(titanic, SEX, CLASS, SURVIVED)

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
with(titanic, Crosstable(SEX, SURVIVED, fisher=TRUE) )

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
with(titanic, Crosstable(SEX, CLASS, SURVIVED, chisq = TRUE))

