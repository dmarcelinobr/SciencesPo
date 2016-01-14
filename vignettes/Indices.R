## ----politicalDiversity1, echo=TRUE, message=FALSE, comment=NA-----------
library("SciencesPo")

# The 1980 presidential election in the US (vote share):

US1980 <- c("Democratic"=0.410, "Republican"=0.507,
              "Independent"=0.066, "Libertarian"=0.011,
              "Citizens"=0.003, "Others"=0.003);

print(US1980)

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

## ----politicalDiversity2, echo=TRUE, message=FALSE, comment=NA-----------
# politicalDiversity(Helsinki$votes); #ENEP Votes

politicalDiversity(Helsinki$seats_SL); #ENP for Saint-Lague

politicalDiversity(Helsinki$seats_DH); #ENP for D'Hondt

## ----table_A.1, echo=TRUE, cache=TRUE------------------------------------
# Table A.l
lijphart <- c("A"=41000, "B"=29000,"C"=17000, "D"=13000)

## ----highestAverages1, message=FALSE, comment=NA-------------------------
highestAverages(parties=names(lijphart), votes=lijphart,
                seats = 6, method = "dh") 

## ----highestAverages3, echo=TRUE, message=FALSE, comment=NA--------------
# Seats allocated using modified Sainte-Lague divisors of 1.4, 3, 5.
highestAverages(parties=names(lijphart), votes=lijphart, 
                seats = 6, method = "msl") 

## ----highestAverages2, echo=TRUE, message=FALSE, comment=NA--------------
highestAverages(parties=names(lijphart), votes=lijphart,
                seats = 6, method = "sl") 

## ----highestAverages4, echo=TRUE, message=FALSE, comment=NA--------------
highestAverages(parties=names(lijphart), votes=lijphart, 
                seats = 6, method = "danish") 

## ----highestAverages5, echo=TRUE, message=FALSE, comment=NA--------------
highestAverages(parties=names(lijphart), votes=lijphart, 
                seats = 6, method = "hill") 

## ----highestAverages6, echo=TRUE, message=FALSE, comment=NA--------------
highestAverages(parties=names(lijphart), votes=lijphart, 
                seats = 6, method = "imperiali") 

## ----highestAverages7, echo=TRUE, message=FALSE, comment=NA--------------

const <- c("A"=100, "B"=150,"C"=300, "D"=400, "E"=50)

highestAverages(parties=names(const), votes=const,
               seats = 3, method = "dh", threshold = 7/100) 

## ----largestRemainders1, eval=FALSE, echo=TRUE, message=FALSE, comment=NA----
#  largestRemainders(parties=names(lijphart), votes=lijphart,
#                  seats = 8, method = "hare")

## ----largestRemainders2, eval=FALSE, echo=TRUE, message=FALSE, comment=NA----
#  largestRemainders(parties=names(lijphart), votes=lijphart,
#                  seats = 8, method = "droop")

## ----data-Italy, eval=FALSE, echo=TRUE, message=FALSE--------------------
#  # The 1946 Italian Constituent Assembly election results: parties and unspoilt votes
#  
#  Italy = data.frame(party=c("DC", "PSIUP", "PCI", "UDN", "UQ", "PRI",
#                              "BNL", "PdA", "MIS", "PCd'I", "CDR",
#                             "PSd'Az", "MUI", "PCS", "PDL", "FDPR"),
#                     votes=c(8101004, 4758129, 4356686, 1560638,	1211956,
#                             1003007, 637328, 334748, 171201, 102393,
#                             97690, 78554, 71021, 51088, 40633, 21853))

## ----largestRemainders3, eval=FALSE, echo=TRUE, message=FALSE, comment=NA----
#  with(Italy, largestRemainders(parties=party, votes=votes,
#                  seats = 556, method = "imperiali.q") )

## ----Ceara-election, echo=TRUE, cache=TRUE-------------------------------
# Results for the state legislative house of Ceara (2014):
Ceara <- c("PCdoB"=187906, "PDT"=326841,"PEN"=132531, "PMDB"=981096,
           "PRB"=2043217,"PSB"=15061, "PSC"=103679, "PSTU"=109830,
           "PTdoB"=213988, "PTC"=67145, "PTN"=278267)

## ----highestAverages8, echo=TRUE, message=FALSE, comment=NA--------------
mytable = highestAverages(parties=names(Ceara), votes=Ceara, 
                seats = 42, method = "dh") 

library(knitr)
kable(mytable, align=c("l","c","c"))

## ----largestRemainders5, eval=FALSE, echo=TRUE, message=FALSE, fig.width=4.5, fig.height=4.5, fig.align="center", fig.cap= "2014 Legislative Election in Ceara (M=42)"----
#  
#  out1 = largestRemainders(parties=names(Ceara), votes=Ceara,
#                  seats = 42, method = "hare")
#  out2 = largestRemainders(parties=names(Ceara), votes=Ceara,
#                  seats = 42, method = "droop")
#  out3 = largestRemainders(parties=names(Ceara), votes=Ceara,
#                  seats = 42, method = "imperiali.q")
#  
#  # add the method:
#  out1$method = "hare"
#  out2$method = "droop"
#  out3$method = "imperiali"
#  
#  data <- rbind(out1, out2, out3)
#  
#  p <- ggplot(data, aes(x=reorder(Party, Seats), y=Seats),
#              fill=method) +
#    geom_bar( position="dodge", stat = "identity") +
#    coord_flip() + labs(x="", y="# Seats")
#  
#  p + theme_grey()

## ----Queensland-election, echo=TRUE, cache=TRUE--------------------------
# 2012 Queensland state elecion:
Queensland <- data.frame(party = c("A", "B", "C", "D", "E", "F"),
                         pvotes = c(49.65, 26.66, 11.5, 7.53, 3.16, 1.47),
                         pseats = c(87.64, 7.87, 2.25, 0.00, 2.25, 0.00))

## ----Quebec-election, echo=TRUE, cache=TRUE------------------------------
# 2012 Quebec provincial election:
Quebec <- data.frame(party = c("PQ", "Lib", "CAQ", "QS", "Option", "Other"),
                         pvotes = c(31.95, 31.20, 27.05, 6.03, 1.89, 1.88),
                         pseats = c(54, 50, 19, 2, 0, 0))

## ----proportionality1, echo=TRUE, message=FALSE, comment=NA--------------
with(Queensland, gallagher(pvotes, pseats))

with(Quebec, gallagher(pvotes, pseats))

## ----proportionality2, echo=TRUE, message=FALSE, comment=NA--------------
with(Queensland, lijphart(pvotes, pseats))

with(Quebec, lijphart(pvotes, pseats))

## ----proportionality3, echo=TRUE, message=FALSE, comment=NA--------------
with(Queensland, grofman(pvotes, pseats))

with(Quebec, grofman(pvotes, pseats))

## ----proportionality4, echo=TRUE, message=FALSE, comment=NA--------------
with(Queensland, farina(pvotes, pseats))

with(Quebec, farina(pvotes, pseats))

## ----proportionality5, echo=TRUE, message=FALSE, comment=NA--------------
with(Queensland, cox.shugart(pvotes, pseats))

with(Quebec, cox.shugart(pvotes, pseats))

## ----proportionality6, echo=TRUE, message=FALSE, comment=NA--------------
with(Queensland, inv.cox.shugart(pvotes, pseats))

with(Quebec, inv.cox.shugart(pvotes, pseats))

