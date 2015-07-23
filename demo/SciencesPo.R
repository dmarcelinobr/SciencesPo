# The SciencesPo demo
# Press <Enter> to advance through the demo,
# Ctrl-C (Linux) or Esc (Windows and Mac) to exit
par(ask=TRUE)
x <- rnorm(100)
y <- x + rnorm(100)
plot(x, y, col=c("steelblue", "indianred"), pch=20)
legendPlotMinimalist("topright", legend=c("Foo", "Bar"), pch=20,
                     col=c("steelblue", "indianred"),
                     horiz=TRUE, bty='n', cex=0.8)


pause()
# Example: 2014 Brazilian election for the lower house. Results from Ceara state:
votes <- c(490205, 1151547, 2449440, 48274, 54403, 173151)
#' # Coalitions leading by the following parties:
parties <- c("DEM","PMDB","PRB","PSB", "PSTU","PTC")
n.seats <-19
dHondt(parties, votes, seats=n.seats)


pause()
# Political Diversity
US2004p <- c(Democratic=0.481, Republican=0.509, Independent=0.0038, Libertarian=0.0032, Constitution=0.0012, Green=0.00096, Others=0.00084)
## concentration indexes
politicalDiversity(US2004p, index= "herfindahl")

pause()
gini.simpson(US2004p)

pause()
politicalDiversity(US2004p, index= "shannon")


pause()
# The Effective Number of Parties
politicalDiversity(US2004p, index= "laakso/taagepera")

pause()

# 2010 Brazilian election outcome (votes):
BR2010v = c(PT=13813587, PMDB=11692384, PSDB=9421347, DEM=6932420, PR=7050274, PP=5987670, PSB=6553345,PDT=4478736, PTB=3808646, PSC=2981714,PV=2886633, PCdoB=2545279, PPS=2376475, PRB=1659973, PMN=1026220,PTdoB=605768, PSOL=968475, PHS=719611, PRTB=283047, PRP=232530, PSL=457490, PTC=563145)

politicalDiversity(BR2010v, index= "laakso/taagepera")


pause()
# 2010 Brazilian election outcome (seats):
BR2010s = c(PT=88, PMDB=79, PSDB=53, DEM=43, PR=41, PP=41, PSB=34,PDT=28, PTB=21, PSC=17,PV=15, PCdoB=15, PPS=12, PRB=8, PMN=4,PTdoB=3, PSOL=3, PHS=2, PRTB=2, PRP=2, PSL=1, PTC=1)

politicalDiversity(BR2010s, index= "laakso/taagepera")

# Using the Golosov index:
politicalDiversity(BR2010s, index= "golosov")

#' # 2010 Election outcome as proportion of seats

pause()

# 2014 Brazilian election outcome (seats):
BR2014s = c(70, 66, 55, 37, 38, 34, 34, 26, 22, 20, 19, 15, 12, 11, 10, 9, 8, 5, 4, 3, 3, 3, 2, 2, 2, 1, 1, 1)

politicalDiversity(BR2010s, index= "laakso/taagepera")

