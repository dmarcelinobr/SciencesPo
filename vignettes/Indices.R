## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(cache=TRUE, echo=TRUE, message=FALSE, comment=NA, fig.align="center", fig.width=5, fig.height=3.5)

## ----table_A.1, echo=TRUE, cache=TRUE------------------------------------
# Table A.l
lijphart <- c("A"=41000, "B"=29000,"C"=17000, "D"=13000)

## ----highestAverages1, message=FALSE, comment=NA-------------------------
library("SciencesPo", quietly = TRUE)

# The d'Hondt will give the same results as Jefferson's method
HighestAverages(parties=names(lijphart),
                votes=lijphart,
                seats = 6, 
                method = "dh") 

## ----highestAverages2, echo=TRUE, message=FALSE, comment=NA--------------
# The Sainte-Laguë will give the same results as the Webster's method (wb)

HighestAverages(parties=names(lijphart),
            votes=lijphart,
                seats = 6,
                method = "sl") 

## ----highestAverages3, echo=TRUE, message=FALSE, comment=NA--------------
HighestAverages(parties=names(lijphart),
                votes=lijphart, 
                seats = 6,
                method = "msl") 

## ----highestAverages4, echo=TRUE, message=FALSE, comment=NA--------------
HighestAverages(parties=names(lijphart), 
                votes=lijphart, 
                seats = 6, 
                method = "danish") 

## ----highestAverages5, echo=TRUE, message=FALSE, comment=NA--------------
HighestAverages(parties=names(lijphart),
                votes=lijphart, 
                seats = 6, 
                method = "hsl") 

## ----highestAverages6, echo=TRUE, message=FALSE, comment=NA--------------
HighestAverages(parties=names(lijphart),
                votes=lijphart, 
                seats = 6, 
                method = "wb") 

## ----highestAverages7, echo=TRUE, message=FALSE, comment=NA--------------
HighestAverages(parties=names(lijphart),
                votes=lijphart, 
                seats = 6, 
                method = "imperiali") 

## ----highestAverages8, echo=TRUE, message=FALSE, comment=NA--------------
Bruges=c("CD&V/N-VA"=32092, "SP.A/Spirit"=20028, 
         "Flemish Interest"=13408, "Open VLD/Vivant"=9520,
         "Green!"=5328, "Other"=2207)

HighestAverages(parties=names(Bruges),
                votes=Bruges, 
                seats = 47,
                method = "imperiali") 

## ----highestAverages9, echo=TRUE, message=FALSE, comment=NA--------------
HighestAverages(parties=names(lijphart),
                votes=lijphart, 
                seats = 6, method = "hh") 

## ----highestAverages10, echo=TRUE, message=FALSE, comment=NA-------------

const <- c("A"=100, "B"=150,"C"=300, "D"=400, "E"=50)

HighestAverages(parties=names(const),
                votes=const,
                seats = 3, method = "dh",
                threshold = 7/100) 

## ----Valencia-election, echo=FALSE, cache=TRUE, comment=NA---------------
(Valencia <- c("PP"=442005, "Podemos"=395729, "PSOE"=275680,
              "C's"=221299, "IU"=68759, "PACMA"=14445, "Others"=35943)) 
blanco=8738
nulo=11891

## ----Valencia, echo=TRUE, message=FALSE, comment=NA----------------------
# Valencia returned 15 members
HighestAverages(parties=names(Valencia),
                votes=Valencia, 
                seats=15, method = "dh",
                threshold = 3/100)

## ----largestRemainders1, echo=TRUE, message=FALSE, comment=NA------------
LargestRemainders(parties=names(lijphart),
                  votes=lijphart, 
                  seats = 8, method = "hare") 

## ----largestRemainders2, echo=TRUE, message=FALSE, comment=NA------------
LargestRemainders(parties=names(lijphart), 
                  votes=lijphart, 
                  seats = 8, method = "droop") 

## ----largestRemainders3, eval=FALSE, echo=TRUE, message=FALSE, comment=NA----
#  LargestRemainders(parties=names(lijphart),
#                    votes=lijphart,
#                    seats = 8, method = "hagb")

## ----data-Italy, echo=TRUE, message=FALSE, comment=NA--------------------
# The 1946 Italian Constituent Assembly election results: parties and unspoiled votes

Italy = data.frame(party=c("DC", "PSIUP", "PCI", "UDN", "UQ", "PRI",
                            "BNL", "PdA", "MIS", "PCd'I", "CDR",
                           "PSd'Az", "MUI", "PCS", "PDL", "FDPR"),
                   votes=c(8101004, 4758129, 4356686, 1560638,	1211956,
                           1003007, 637328, 334748, 171201, 102393,
                           97690, 78554, 71021, 51088, 40633, 21853))

## ----largestRemainders4, eval=FALSE, echo=TRUE, message=FALSE, comment=NA----
#  with(Italy, LargestRemainders(parties=party,
#                                votes=votes, seats = 556,
#                                method = "imperiali") )

## ----largestRemainders5, eval=FALSE, echo=TRUE, message=FALSE, comment=NA----
#  with(Italy, LargestRemainders(parties=party,
#                                votes=votes, seats = 556,
#                                method = "imperiali.adj") )

## ----Ceara-election, echo=TRUE, cache=TRUE, message=FALSE, comment=NA----
# Results for the state legislative house of Ceará (2014):
Ceara <- c("PCdoB"=187906, "PDT"=326841,"PEN"=132531, "PMDB"=981096,
           "PRB"=2043217,"PSB"=15061, "PSC"=103679, "PSTU"=109830,
           "PTdoB"=213988, "PTC"=67145, "PTN"=278267)

## ----highestAverages11, echo=TRUE, message=FALSE, comment=NA-------------
mytable = HighestAverages(parties=names(Ceara), 
                          votes=Ceara,
                          seats = 42, method = "dh") 

library(knitr)

kable(mytable, align=c("l","c","c"), caption="Outcome under d'Hondt")

## ----highestAverages12, echo=TRUE, message=FALSE, comment=NA, fig.width=6.5, fig.height=4.5, fig.align="center"----

gg <- ggplot()
gg <- gg +  geom_bar( data = mytable, aes(x = reorder(Party, -Seats), y = Seats, fill = Party), stat = "identity", alpha=.9)
gg <- gg + geom_label(data = mytable,
aes(x = reorder(Party, -Seats), y= Seats + 1, label = Seats), hjust = .5)
gg <- gg + scale_y_continuous(expand = c(0, 0), limits = c(0, 25))
gg <- gg + scale_fill_party("BRA")
gg <- gg + labs(list(x = "", y = "# Seats obtained",
title = "The 2014 election in the Ceará State (seats won by party)"))
gg <-  gg + theme_fte(base_size = 10, base_family = "Tahoma")
gg

## ----echo=TRUE, message=FALSE, comment=NA, fig.width=6.5, fig.height=4.5, fig.align="center", fig.cap= "2014 Legislative Election in Ceará (M=42)"----

out1 = HighestAverages(
  parties = names(Ceara),
  votes = Ceara,
  seats = 42,
  method = "dh"
  )
  out2 = HighestAverages(
  parties = names(Ceara),
  votes = Ceara,
  seats = 42,
  method = "imperiali"
  )
  out3 = HighestAverages(
  parties = names(Ceara),
  votes = Ceara,
  seats = 42,
  method = "sl"
  )
  
  # add the method:
  out1$Method = "d'Hondt"
  out2$Method = "Imperiali"
  out3$Method = "Saint-Laguë"
  
 data <- rbind(out1, out2, out3)

## ----echo=FALSE, message=FALSE, comment=NA, fig.width=7, fig.height=4.5, fig.align="center", fig.cap= "2014 Legislative Election in Ceará (M=42)"----
  
  p <- ggplot()
  p <- p + geom_bar(
  data = data,
  aes(
  x = reorder(Party, -Seats),
  y = Seats,
  fill = Method
  ), 
  stat = "identity",
  position = position_dodge()
  )
  p <- p + labs(x = "", y = "# Seats obtained", title="The 2014 election in the Ceará State (seats won by party)")
  p <- p  + scale_fill_pub("fte")
  p <- p + theme_fte(legend = "top", base_size = 10, base_family = "Tahoma")
  p  + theme(panel.grid.major.x=element_blank()) 


## ----largestRemainders7, eval=FALSE, echo=FALSE, message=FALSE, comment=NA----
#  
#  # 2014 Federal elections, 30 seats to be returned in the state of Parana, Brazil.
#  
#  PR = c(
#    "PSDB/DEM/PR/PSC/PTdoB/PP/SD/PSD/PPS" = 2601709,
#    "PT/PDT/PRB/PTN/PCdoB" = 1109905,
#    "PSDC/PEN/PTB/PHS/PMN/PROS" = 501148,
#    "PV/PPL" = 280767
#    )

## ----largestRemainders8, eval=FALSE, echo=FALSE, message=FALSE, comment=NA----
#  
#  # 2014 Federal elections, 70 seats to be returned in the state of Sao Paulo, Brazil.
#  
#  SP = c(
#    "PSDB/DEM/PPS" = 5537630,
#    "PT/PCdoB" = 3170003,
#    "PMDB/PROS/PP/PSD" = 2384740,
#    "PSOL/PSTU" = 462992,
#    "PSL/PTN/PMN/PTC/PTdoB" = 350186,
#    "PHS/PRP" = 252205
#    )
#  

## ----politicalDiversity1, echo=FALSE, cache=TRUE, message=FALSE, comment=NA----
# The 2004 presidential election in the US (vote share):

US2004 <- c(
  "Democratic" = 0.481,
  "Republican" = 0.509,
  "Independent" = 0.0038,
  "Libertarian" = 0.0032,
  "Constitution" = 0.0012,
  "Green" = 0.00096,
  "Others" = 0.00084
  )

print(US2004)

## ----politicalDiversity2, echo=TRUE, message=FALSE, comment=NA-----------
PoliticalDiversity(US2004); # ENEP (laakso/taagepera) method 

## ----politicalDiversity3, echo=TRUE, message=FALSE, comment=NA-----------

PoliticalDiversity(US2004, index= "golosov");

## ----politicalDiversity4, echo=TRUE, message=FALSE, comment=NA-----------

PoliticalDiversity(US2004, index= "inv.herfindahl");

# Compares to:
Herfindahl(US2004)

## ----Helsinki-election, echo=FALSE, cache=TRUE, message=FALSE, comment=NA----
# Helsinki's 1999:
Helsinki <- 
  data.frame(
  votes = c(68885, 18343, 86448, 21982, 51587,
            27227, 8482, 7250, 365, 2734, 1925,
            475, 1693, 693, 308, 980, 560, 590, 185),
  seats.SL=c(5, 1, 6, 1, 4, 2, 1, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0),
  seats.dH=c(5, 1, 7, 1, 4, 2, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0))

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
# Helsinki's 1999 election:
Helsinki


## ----politicalDiversity5, echo=TRUE, message=FALSE, comment=NA-----------

PoliticalDiversity(Helsinki$votes); #ENP given votes

PoliticalDiversity(Helsinki$seats.SL); #ENP for Saint-Lague 

PoliticalDiversity(Helsinki$seats.dH); #ENP for D'Hondt

## ----Queensland-election, echo=FALSE, cache=TRUE, message=FALSE, comment=NA----
# 2012 Queensland state elecion:
Queensland <-
data.frame(
party = c("LNP", "ALP", "Katter", "Greens", "Ind", "Others"),
votes = c(1214553, 652092, 282098, 184147, 77282, 35794),
pvotes = c(49.65, 26.66, 11.5, 7.53, 3.16, 1.47),
seats = c(78, 7, 2, 0, 2, 0),
pseats = c(87.64, 7.87, 2.25, 0.00, 2.25, 0.00)
)

## ----Quebec-election, echo=FALSE, cache=TRUE, message=FALSE, comment=NA----
# 2012 Quebec provincial election:
Quebec <-
  data.frame(
  party = c("PQ", "Lib", "CAQ", "QS", "Option", "Green", "Others"),
  votes = c(1393703, 1360968, 1180235, 263111, 82539, 43394, 38738),
  pvotes = c(31.95, 31.20, 27.05, 6.03, 1.89, 0.99, 0.89),
  seats = c(54, 50, 19, 2, 0, 0, 0),
  pseats =  c(43.2, 40, 15.2, 1.6, 0, 0, 0)
  )


## ----Queensland, echo=TRUE, message=FALSE, comment=NA--------------------
# 2012 Queensland state elecion:
Queensland

## ----Quebec, echo=TRUE, message=FALSE, comment=NA------------------------
# 2012 Quebec provincial election:
Quebec

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------

with(Queensland, Proportionality(pvotes, pseats, 
                     index = "Rae") )

with(Quebec, Proportionality(pvotes, pseats, 
                     index = "Rae") )

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------

with(Queensland, Proportionality(pvotes, pseats, 
                     index = "Loosemore-Hanby") )

with(Quebec, Proportionality(pvotes, pseats, 
                     index = "Loosemore-Hanby") )

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------

with(Queensland, Proportionality(pvotes, pseats, 
                     index = "Gallagher") )

with(Quebec, Proportionality(pvotes, pseats, 
                     index = "Gallagher") )

## The inverted version
with(Quebec, Proportionality(pvotes, pseats, 
                     index = "inv.Gallagher") )

## ----eval=FALSE, echo=FALSE, message=FALSE, comment=NA-------------------
#  
#  with(Queensland, Proportionality(pvotes, pseats,
#                       index = "mod.Gallagher") )
#  
#  with(Quebec, Proportionality(pvotes, pseats,
#                       index = "mod.Gallagher") )
#  
#  ## The inverted version
#  with(Quebec, Proportionality(pvotes, pseats,
#                       index = "mod.inv.Gallagher") )

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------

with(Queensland, Proportionality(pvotes, pseats, 
                     index = "Rose") )

with(Quebec, Proportionality(pvotes, pseats, 
                     index = "Rose") )

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------

with(Queensland, Proportionality(pvotes, pseats, 
                     index = "Grofman") )

with(Quebec, Proportionality(pvotes, pseats, 
                     index = "Grofman") )

## The inverted version:
with(Quebec, Proportionality(pvotes, pseats, 
                     index = "inv.Grofman") )

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------

with(Queensland, Proportionality(pvotes, pseats, 
                     index = "Lijphart") )


with(Quebec, Proportionality(pvotes, pseats, 
                     index = "Lijphart") )


## The inverted version:
with(Quebec, Proportionality(pvotes, pseats, 
                     index = "invlijphart") )


## ----echo=TRUE, message=FALSE, comment=NA--------------------------------

with(Queensland, Proportionality(pvotes, pseats, 
                     index = "Farina") )

with(Quebec, Proportionality(pvotes, pseats, 
                     index = "Farina") )


## ----echo=TRUE, message=FALSE, comment=NA--------------------------------

with(Queensland, Proportionality(pvotes, pseats, 
                     index = "Cox-Shugart") )

with(Quebec, Proportionality(pvotes, pseats, 
                     index = "Cox-Shugart") )

## The inverted version:
with(Quebec, Proportionality(pvotes, pseats, 
                     index = "inv.Cox-Shugart") )

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------

with(Queensland, Proportionality(pvotes, pseats, 
                     index = "Sainte-Lague") )

with(Quebec, Proportionality(pvotes, pseats, 
                     index = "Sainte-Lague") )

with(Quebec, Proportionality(pvotes, pseats, 
                     index = "inv.Sainte-Lague") )

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------

with(Queensland, Proportionality(pvotes, pseats, 
                     index = "DHondt") )

with(Quebec, Proportionality(pvotes, pseats, 
                     index = "DHondt") )

## ----echo=FALSE, message=FALSE, comment=NA-------------------------------
sessionInfo()

