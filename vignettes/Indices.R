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

## ----highestAverages1, echo=TRUE, message=FALSE, comment=NA--------------
highestAverages(parties=names(Ceara), votes=Ceara,
                seats = 42, method = "dh") 

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
highestAverages(parties=names(Ceara), votes=Ceara,
                seats = 42, method = "sl") 

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
highestAverages(parties=names(Ceara), votes=Ceara, 
                seats = 42, method = "msl") 

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
highestAverages(parties=names(Ceara), votes=Ceara, 
                seats = 42, method = "hill") 

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
highestAverages(parties=names(Ceara), votes=Ceara, 
                seats = 42, method = "imperiali") 

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------

const <- c("A"=100, "B"=150,"C"=300, "D"=400, "E"=50)

highestAverages(parties=names(const), votes=const,
               seats = 3, method = "dh", threshold = 7/100) 

## ----eval=FALSE, echo=TRUE, message=FALSE, comment=NA--------------------
#  largestRemainders(parties=names(Ceara), votes=Ceara,
#                  seats = 42, method = "hare")

## ----eval=FALSE, echo=TRUE, message=FALSE, comment=NA--------------------
#  largestRemainders(parties=names(Ceara), votes=Ceara,
#                  seats = 42, method = "droop")

## ----data-Italy, eval=FALSE, echo=TRUE, message=FALSE--------------------
#  
#  # The 1946 Italian Constituent Assembly election results: parties and unspoilt votes
#  
#  Italy = data.frame(party=c("DC", "PSIUP", "PCI", "UDN", "UQ", "PRI",
#                              "BNL", "PdA", "MIS", "PCd'I", "CDR",
#                             "PSd'Az", "MUI", "PCS", "PDL", "FDPR"),
#                     votes=c(8101004, 4758129, 4356686, 1560638,	1211956,
#                             1003007, 637328, 334748, 171201, 102393,
#                             97690, 78554, 71021, 51088, 40633, 21853))

## ----eval=FALSE, echo=TRUE, message=FALSE, comment=NA--------------------
#  with(Italy, largestRemainders(parties=party, votes=votes,
#                  seats = 556, method = "imperiali.q") )

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
mytable = highestAverages(parties=names(Ceara), votes=Ceara, 
                seats = 42, method = "dh") 

library(knitr)

kable(mytable, align=c("l","c","c"))

## ----echo=TRUE, message=FALSE, fig.width=4.5, fig.height=4.5, fig.align="center", fig.cap= "2014 Legislative Election in Ceara (M=42)"----

mytable = highestAverages(parties=names(Ceara), votes=Ceara, 
                seats = 42, method = "dh") 

p <- ggplot(mytable, aes(x=reorder(Party, Seats), y=Seats)) + 
  geom_bar( position="dodge", stat = "identity") +
  coord_flip() + labs(x="", y="# Seats")

p + theme_grey() 

