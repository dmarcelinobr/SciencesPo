# The SciencesPo demo
# Press <Enter> to advance through the demo,
# Ctrl-C (Linux) or Esc (Windows and Mac) to exit

# Example: 2014 Brazilian election for the lower house. Results from Ceara state:
votes <- c(490205, 1151547, 2449440, 48274, 54403, 173151)
#' # Coalitions leading by the following parties:
parties <- c("DEM","PMDB","PRB","PSB", "PSTU","PTC")
n.seats <-19
dHondt(parties, votes, seats=n.seats)



.Pause()
# Political Diversity
US2004p <- c(Democratic=0.481, Republican=0.509, Independent=0.0038, Libertarian=0.0032, Constitution=0.0012, Green=0.00096, Others=0.00084)


# The Effective Number of Parties
PoliticalDiversity(US2004p, index= "laakso/taagepera")


# The Effective Number of Parties
PoliticalDiversity(US2004p, index= "golosov")

.Pause()

# 2010 Brazilian election outcome (votes):
BR2010v = c(PT=13813587, PMDB=11692384, PSDB=9421347, DEM=6932420, PR=7050274, PP=5987670, PSB=6553345,PDT=4478736, PTB=3808646, PSC=2981714,PV=2886633, PCdoB=2545279, PPS=2376475, PRB=1659973, PMN=1026220,PTdoB=605768, PSOL=968475, PHS=719611, PRTB=283047, PRP=232530, PSL=457490, PTC=563145)


# 2010 Brazilian election outcome (seats):
BR2010s = c(PT=88, PMDB=79, PSDB=53, DEM=43, PR=41, PP=41, PSB=34,PDT=28, PTB=21, PSC=17,PV=15, PCdoB=15, PPS=12, PRB=8, PMN=4,PTdoB=3, PSOL=3, PHS=2, PRTB=2, PRP=2, PSL=1, PTC=1)

.Pause()
PoliticalDiversity(BR2010s, index= "laakso/taagepera")


# Using Golosov's index:
PoliticalDiversity(BR2010s, index= "golosov")



.Pause()
# Proportionality measures
Proportionality(BR2010v, BR2010s, "Loosemore-Hanby")


Proportionality(BR2010v, BR2010s, "Gallagher")


.Pause()
# Descriptive

## Simulating the FREQ procedure of SPSS.
data("cathedrals")

# Descriptive summaries for an entire data frame
Describe(cathedrals)

.Pause()
# Frequency table
Freq(titanic, SEX)

.Pause()
## Cross-Tabulation: two-way
with(titanic, Crosstable(SEX, SURVIVED))


.Pause()
## Cross-Tabulation: three-way
with(titanic, Crosstable(SEX, AGE, SURVIVED))


.Pause()
# Color scales

scales::show_col(pub_color_pal("pub12")(12))


.Pause()

# Colors of political parties
party_color_pal("BRA", plot=TRUE)

.Pause()
party_color_pal("ARG", plot=TRUE)

.Pause()
party_color_pal("CAN", plot=TRUE)
