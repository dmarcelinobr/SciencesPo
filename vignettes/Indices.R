<<<<<<< HEAD
=======
## ----table_A.1, echo=TRUE, cache=TRUE------------------------------------
# Table A.l
lijphart <- c("A"=41000, "B"=29000,"C"=17000, "D"=13000)

>>>>>>> 6ed3a017772826c3db1ae904c6b601a390c5961b
## ----highestAverages1, message=FALSE, comment=NA-------------------------
library("SciencesPo", quietly = TRUE)

# The d'Hondt will give the same results as Jefferson's method
<<<<<<< HEAD
HighestAverages(parties=names(lijphart),
=======
highestAverages(parties=names(lijphart),
>>>>>>> 6ed3a017772826c3db1ae904c6b601a390c5961b
                votes=lijphart,
                seats = 6, 
                method = "dh") 

## ----highestAverages2, echo=TRUE, message=FALSE, comment=NA--------------
# The Sainte-Laguë will give the same results as the Webster's method (wb)
<<<<<<< HEAD
HighestAverages(parties=names(lijphart),
=======
highestAverages(parties=names(lijphart),
>>>>>>> 6ed3a017772826c3db1ae904c6b601a390c5961b
                votes=lijphart,
                seats = 6,
                method = "sl") 

## ----highestAverages3, echo=TRUE, message=FALSE, comment=NA--------------
<<<<<<< HEAD
HighestAverages(parties=names(lijphart),
=======
highestAverages(parties=names(lijphart),
>>>>>>> 6ed3a017772826c3db1ae904c6b601a390c5961b
                votes=lijphart, 
                seats = 6,
                method = "msl") 

## ----highestAverages4, echo=TRUE, message=FALSE, comment=NA--------------
<<<<<<< HEAD
HighestAverages(parties=names(lijphart), 
=======
highestAverages(parties=names(lijphart), 
>>>>>>> 6ed3a017772826c3db1ae904c6b601a390c5961b
                votes=lijphart, 
                seats = 6, 
                method = "danish") 

## ----highestAverages5, echo=TRUE, message=FALSE, comment=NA--------------
<<<<<<< HEAD
HighestAverages(parties=names(lijphart),
=======
highestAverages(parties=names(lijphart),
>>>>>>> 6ed3a017772826c3db1ae904c6b601a390c5961b
                votes=lijphart, 
                seats = 6, 
                method = "hsl") 

## ----highestAverages6, echo=TRUE, message=FALSE, comment=NA--------------
<<<<<<< HEAD
HighestAverages(parties=names(lijphart),
=======
highestAverages(parties=names(lijphart),
>>>>>>> 6ed3a017772826c3db1ae904c6b601a390c5961b
                votes=lijphart, 
                seats = 6, 
                method = "wb") 

## ----highestAverages7, echo=TRUE, message=FALSE, comment=NA--------------
<<<<<<< HEAD
HighestAverages(parties=names(lijphart),
=======
highestAverages(parties=names(lijphart),
>>>>>>> 6ed3a017772826c3db1ae904c6b601a390c5961b
                votes=lijphart, 
                seats = 6, 
                method = "imperiali") 

## ----highestAverages8, echo=TRUE, message=FALSE, comment=NA--------------
Bruges=c("CD&V/N-VA"=32092, "SP.A/Spirit"=20028, 
         "Flemish Interest"=13408, "Open VLD/Vivant"=9520,
         "Green!"=5328, "Other"=2207)

<<<<<<< HEAD
HighestAverages(parties=names(Bruges),
=======
highestAverages(parties=names(Bruges),
>>>>>>> 6ed3a017772826c3db1ae904c6b601a390c5961b
                votes=Bruges, 
                seats = 47,
                method = "imperiali") 

## ----highestAverages9, echo=TRUE, message=FALSE, comment=NA--------------
<<<<<<< HEAD
HighestAverages(parties=names(lijphart),
=======
highestAverages(parties=names(lijphart),
>>>>>>> 6ed3a017772826c3db1ae904c6b601a390c5961b
                votes=lijphart, 
                seats = 6, method = "hh") 

## ----highestAverages10, echo=TRUE, message=FALSE, comment=NA-------------

const <- c("A"=100, "B"=150,"C"=300, "D"=400, "E"=50)

<<<<<<< HEAD
HighestAverages(parties=names(const),
=======
highestAverages(parties=names(const),
>>>>>>> 6ed3a017772826c3db1ae904c6b601a390c5961b
                votes=const,
                seats = 3, method = "dh",
                threshold = 7/100) 

<<<<<<< HEAD
## ----Valencia, echo=TRUE, message=FALSE, comment=NA----------------------
# Valencia returned 15 members
HighestAverages(parties=names(Valencia),
=======
## ----Valencia-election, echo=FALSE, cache=TRUE, comment=NA---------------
(Valencia <- c("PP"=442005, "Podemos"=395729, "PSOE"=275680,
              "C's"=221299, "IU"=68759, "PACMA"=14445, "Others"=35943)) 
blanco=8738
nulo=11891

## ----Valencia, echo=TRUE, message=FALSE, comment=NA----------------------
# Valencia returned 15 members
highestAverages(parties=names(Valencia),
>>>>>>> 6ed3a017772826c3db1ae904c6b601a390c5961b
                votes=Valencia, 
                seats=15, method = "dh",
                threshold = 3/100)

## ----largestRemainders1, eval=FALSE, echo=TRUE, message=FALSE, comment=NA----
<<<<<<< HEAD
#  LargestRemainders(parties=names(lijphart),
=======
#  largestRemainders(parties=names(lijphart),
>>>>>>> 6ed3a017772826c3db1ae904c6b601a390c5961b
#                    votes=lijphart,
#                    seats = 8, method = "hare")

## ----largestRemainders2, eval=FALSE, echo=TRUE, message=FALSE, comment=NA----
<<<<<<< HEAD
#  LargestRemainders(parties=names(lijphart),
=======
#  largestRemainders(parties=names(lijphart),
>>>>>>> 6ed3a017772826c3db1ae904c6b601a390c5961b
#                    votes=lijphart,
#                    seats = 8, method = "droop")

## ----largestRemainders3, eval=FALSE, echo=TRUE, message=FALSE, comment=NA----
<<<<<<< HEAD
#  LargestRemainders(parties=names(lijphart),
=======
#  largestRemainders(parties=names(lijphart),
>>>>>>> 6ed3a017772826c3db1ae904c6b601a390c5961b
#                    votes=lijphart,
#                    seats = 8, method = "hagb")

## ----data-Italy, eval=FALSE, echo=TRUE, message=FALSE--------------------
#  # The 1946 Italian Constituent Assembly election results: parties and unspoilt votes
#  
#  Italy = data.frame(party=c("DC", "PSIUP", "PCI", "UDN", "UQ", "PRI",
#                              "BNL", "PdA", "MIS", "PCd'I", "CDR",
#                             "PSd'Az", "MUI", "PCS", "PDL", "FDPR"),
#                     votes=c(8101004, 4758129, 4356686, 1560638,	1211956,
#                             1003007, 637328, 334748, 171201, 102393,
#                             97690, 78554, 71021, 51088, 40633, 21853))

## ----largestRemainders4, eval=FALSE, echo=TRUE, message=FALSE, comment=NA----
<<<<<<< HEAD
#  with(Italy, LargestRemainders(parties=party,
#                                votes=votes, seats = 556,
#                                method = "imperiali.q") )

## ----highestAverages11, echo=TRUE, message=FALSE, comment=NA-------------
mytable = HighestAverages(parties=names(Ceara), 
=======
#  with(Italy, largestRemainders(parties=party,
#                                votes=votes, seats = 556,
#                                method = "imperiali.q") )

## ----Ceara-election, echo=TRUE, cache=TRUE-------------------------------
# Results for the state legislative house of Ceará (2014):
Ceara <- c("PCdoB"=187906, "PDT"=326841,"PEN"=132531, "PMDB"=981096,
           "PRB"=2043217,"PSB"=15061, "PSC"=103679, "PSTU"=109830,
           "PTdoB"=213988, "PTC"=67145, "PTN"=278267)

## ----highestAverages11, echo=TRUE, message=FALSE, comment=NA-------------
mytable = highestAverages(parties=names(Ceara), 
>>>>>>> 6ed3a017772826c3db1ae904c6b601a390c5961b
                          votes=Ceara,
                          seats = 42, method = "dh") 

library(knitr)

kable(mytable, align=c("l","c","c"), caption="Outcome under d'Hondt")

<<<<<<< HEAD
## ----highestAverages12, echo=TRUE, message=FALSE, comment=NA, fig.width=5, fig.height=4.5, fig.align="center"----

gg <- ggplot()
gg <- gg +  geom_lollipop(data=mytable, aes(x = reorder(Party, Seats), y = Seats, color = Party), point.size = 3)
gg <- gg + geom_label(data=mytable, aes(x = reorder(Party, Seats), y = Seats + .5, label = Seats), hjust = 0) 
gg <-  gg + scale_y_continuous(expand = c(0, 0), limits = c(0, 25))
gg <- gg + scale_color_parties("BRA") + scale_fill_parties("BRA")
gg <- gg + coord_flip()
gg <-  gg + labs(list(x = "", y = "Number of seats obtained",
            title="The 2014 Brazilian elections in Ceará, \n Seats won by party"))
gg <-  gg + theme_fte(horizontal = FALSE)
gg <-  gg + theme(panel.grid.major.y=element_blank()) 
gg <-  gg + theme(legend.position = "none") 
gg


## ----largestRemainders5, eval=TRUE, echo=TRUE, message=FALSE, fig.width=6, fig.height=3.5, fig.align="center", fig.cap= "2014 Legislative Election in Ceará (M=42)"----

out1 = HighestAverages(parties=names(Ceara), votes=Ceara, 
                seats = 42, method = "dh")
out2 = HighestAverages(parties=names(Ceara), votes=Ceara, 
                seats = 42, method = "imperiali") 
out3 = HighestAverages(parties=names(Ceara), votes=Ceara, 
=======
## ----highestAverages12, echo=TRUE, message=FALSE, comment=NA, fig.width=5, fig.height=4, fig.align="center"----
p <- ggplot(mytable) + 
  geom_point(aes(x = reorder(Party, Seats), y = Seats, color = Party), stat = "identity", size = 5) + 
  geom_text(aes(x = reorder(Party, Seats), y = Seats + .5, label = Seats), hjust = 0, vjust = -0.5) + 
  geom_bar(aes(x = reorder(Party, Seats), y = Seats, fill = Party), stat = "identity", width = 0.05) + 
  coord_flip() + 
  labs(list(y = "Number of seats obtained", x = "",
            title="Seats won by party, 2014 Brazilian elections in Ceará")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 25)) + 
  scale_color_parties("BRA") + scale_fill_parties("BRA") +
  theme_fte() + 
  theme(legend.position = "none") 
p

## ----largestRemainders5, eval=TRUE, echo=TRUE, message=FALSE, fig.width=6, fig.height=3.5, fig.align="center", fig.cap= "2014 Legislative Election in Ceará (M=42)"----

out1 = highestAverages(parties=names(Ceara), votes=Ceara, 
                seats = 42, method = "dh")
out2 = highestAverages(parties=names(Ceara), votes=Ceara, 
                seats = 42, method = "imperiali") 
out3 = highestAverages(parties=names(Ceara), votes=Ceara, 
>>>>>>> 6ed3a017772826c3db1ae904c6b601a390c5961b
                seats = 42, method = "sl")

# add the method:
out1$Method = "d'Hondt"
out2$Method = "imperiali"
out3$Method = "Saint-Laguë"


data <- rbind(out1, out2, out3)

p = ggplot(data=data, aes(x=reorder(Party, -Seats), y=Seats, fill=Method)) +
    geom_bar(stat="identity",position=position_dodge()) +
   labs(x="", y="Seats")
p + scale_fill_fte() + 
  theme_538(legend = "top", base_size = 10) 

## ----largestRemainders6, eval=FALSE, echo=TRUE, message=FALSE, fig.width=7, fig.height=4.5, fig.align="center", fig.cap= "2014 Legislative Election in Ceara (M=42)"----
#  
#  #2014 Federal elections, 30 seats to be returned in the state of Parana, Brazil.
#  
#  PR=c("PSDB/DEM/PR/PSC/PTdoB/PP/SD/PSD/PPS"=2601709,
#       "PT/PDT/PRB/PTN/PCdoB"=1109905,
#       "PSDC/PEN/PTB/PHS/PMN/PROS"=501148,
#       "PV/PPL"=280767)

## ----largestRemainders7, eval=FALSE, echo=TRUE, message=FALSE, fig.width=7, fig.height=4.5, fig.align="center", fig.cap= "2014 Legislative Election in Ceara (M=42)"----
#  
#  2014 Federal elections, 70 seats to be returned in the state of Sao Paulo, Brazil.
#  SP=c("PSDB/DEM/PPS"=5537630, "PT/PCdoB"=3170003,
#       "PMDB/PROS/PP/PSD"=2384740, "PSOL/PSTU"=462992,
#       "PSL/PTN/PMN/PTC/PTdoB"=350186, "PHS/PRP"=252205)
#  
#  

## ----politicalDiversity1, echo=TRUE, message=FALSE, comment=NA-----------
# The 2004 presidential election in the US (vote share):

US2004 <- c("Democratic"=0.481, "Republican"=0.509, 
            "Independent"=0.0038, "Libertarian"=0.0032, 
            "Constitution"=0.0012, "Green"=0.00096,
            "Others"=0.00084)

print(US2004)

## ----politicalDiversity2, echo=TRUE, message=FALSE, comment=NA-----------
<<<<<<< HEAD
PoliticalDiversity(US2004); # ENEP (laakso/taagepera) method 

## ----politicalDiversity3, echo=TRUE, message=FALSE, comment=NA-----------
PoliticalDiversity(US2004, index= "golosov");

## ----politicalDiversity4, echo=TRUE, message=FALSE, comment=NA-----------
PoliticalDiversity(US2004, index= "herfindahl");
=======
politicalDiversity(US2004); # ENEP (laakso/taagepera) method 

## ----politicalDiversity3, echo=TRUE, message=FALSE, comment=NA-----------
politicalDiversity(US2004, index= "golosov");

## ----politicalDiversity4, echo=TRUE, message=FALSE, comment=NA-----------
politicalDiversity(US2004, index= "herfindahl");
>>>>>>> 6ed3a017772826c3db1ae904c6b601a390c5961b

## ----Helsinki-election, echo=TRUE, message=FALSE-------------------------
# Helsinki's 1999

Helsinki <- data.frame(
  votes = c(68885, 18343, 86448, 21982, 51587,
            27227, 8482, 7250, 365, 2734, 1925,
            475, 1693, 693, 308, 980, 560, 590, 185),
  seats.SL=c(5, 1, 6, 1, 4, 2, 1, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0),
  seats.dH=c(5, 1, 7, 1, 4, 2, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0))

## ----politicalDiversity5, echo=TRUE, message=FALSE, comment=NA-----------
<<<<<<< HEAD
# PoliticalDiversity(Helsinki$votes); #ENEP Votes

PoliticalDiversity(Helsinki$seats.SL); #ENP for Saint-Lague

PoliticalDiversity(Helsinki$seats.dH); #ENP for D'Hondt

## ----eval=FALSE, proportionality1, echo=TRUE, message=FALSE, comment=NA----
#  
#  with(Queensland, Gallagher(pvotes, pseats))
#  
#  with(Quebec, Gallagher(pvotes, pseats))
#  
#  # The same as above
#  with(Queensland, Proportionality(pvotes, pseats,
#                       index = "Gallagher") )

## ----eval=FALSE, proportionality2, echo=TRUE, message=FALSE, comment=NA----
#  with(Queensland, Lijphart(pvotes, pseats))
#  
#  with(Quebec, Lijphart(pvotes, pseats))
#  
#  # The same as above
#  with(Queensland, Proportionality(pvotes, pseats,
#                       index = "Lijphart") )

## ----eval=FALSE, proportionality3, echo=TRUE, message=FALSE, comment=NA----
#  with(Queensland, Grofman(pvotes, pseats))
#  
#  with(Quebec, Grofman(pvotes, pseats))
#  
#  # The same as above
#  with(Queensland, Proportionality(pvotes, pseats,
#                       index = "Grofman") )

## ----eval=FALSE, proportionality4, echo=TRUE, message=FALSE, comment=NA----
#  with(Queensland, Farina(pvotes, pseats))
#  
#  with(Quebec, Farina(pvotes, pseats))
#  
#  # The same as above
#  with(Queensland, Proportionality(pvotes, pseats,
#                       index = "Farina") )

## ----eval=FALSE, proportionality5, echo=TRUE, message=FALSE, comment=NA----
#  with(Queensland, CoxShugart(pvotes, pseats))
#  
#  with(Quebec, CoxShugart(pvotes, pseats))
#  
#  # The same as above
#  with(Queensland, Proportionality(pvotes, pseats,
#                       index = "Cox-Shugart") )

## ----proportionality6, echo=TRUE, message=FALSE, comment=NA--------------
with(Queensland, inv.CoxShugart(pvotes, pseats))

with(Quebec, inv.CoxShugart(pvotes, pseats))
=======
# politicalDiversity(Helsinki$votes); #ENEP Votes

politicalDiversity(Helsinki$seats.SL); #ENP for Saint-Lague

politicalDiversity(Helsinki$seats.dH); #ENP for D'Hondt

## ----Queensland-election, echo=TRUE, cache=TRUE--------------------------
# 2012 Queensland state elecion:
Queensland <- data.frame(party = c("LNP", "ALP", "Katter", "Greens", "Ind", "Others"),
                         votes = c(1214553,652092,282098,184147,77282,35794),
                         pvotes = c(49.65, 26.66, 11.5, 7.53, 3.16, 1.47),
                         seats = c(78, 7, 2, 0, 2, 0),
                         pseats = c(87.64, 7.87, 2.25, 0.00, 2.25, 0.00))

## ----Quebec-election, echo=TRUE, cache=TRUE------------------------------
# 2012 Quebec provincial election:
Quebec <- data.frame(party = c("PQ", "Lib", "CAQ", "QS", "Option", "Other"),
                         pvotes = c(31.95, 31.20, 27.05, 6.03, 1.89, 1.88),
                         pseats = c(54, 50, 19, 2, 0, 0))

## ----proportionality1, echo=TRUE, message=FALSE, comment=NA--------------

with(Queensland, gallagher(pvotes, pseats))

with(Quebec, gallagher(pvotes, pseats))

# The same as above
with(Queensland, proportionality(pvotes, pseats, 
                     index = "gallagher") )

## ----proportionality2, echo=TRUE, message=FALSE, comment=NA--------------
with(Queensland, lijphart(pvotes, pseats))

with(Quebec, lijphart(pvotes, pseats))

# The same as above
with(Queensland, proportionality(pvotes, pseats, 
                     index = "lijphart") )

## ----proportionality3, echo=TRUE, message=FALSE, comment=NA--------------
with(Queensland, grofman(pvotes, pseats))

with(Quebec, grofman(pvotes, pseats))

# The same as above
with(Queensland, proportionality(pvotes, pseats, 
                     index = "grofman") )

## ----proportionality4, echo=TRUE, message=FALSE, comment=NA--------------
with(Queensland, farina(pvotes, pseats))

with(Quebec, farina(pvotes, pseats))

# The same as above
with(Queensland, proportionality(pvotes, pseats, 
                     index = "farina") )

## ----proportionality5, echo=TRUE, message=FALSE, comment=NA--------------
with(Queensland, cox.shugart(pvotes, pseats))

with(Quebec, cox.shugart(pvotes, pseats))

# The same as above
with(Queensland, proportionality(pvotes, pseats, 
                     index = "cox.shugart") )

## ----proportionality6, echo=TRUE, message=FALSE, comment=NA--------------
with(Queensland, inv.cox.shugart(pvotes, pseats))

with(Quebec, inv.cox.shugart(pvotes, pseats))
>>>>>>> 6ed3a017772826c3db1ae904c6b601a390c5961b

## ----eval=FALSE, echo=FALSE, message=FALSE, comment=NA-------------------
#  sessionInfo()

