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
party_2004 <- c("Democratic", "Republican", "Independent", "Libertarian", "Constitution", "Green", "Others")
 US2004 <- c(0.481, 0.509, 0.0038, 0.0032, 0.0012, 0.00096, 0.00084)
 ## concentration indexes
 politicalDiversity(US2004, index= "herfindahl")

 pause()
 gini.simpson(US2004)

 pause()
 politicalDiversity(US2004, index= "shannon")


 pause()
 # The Effective Number of Parties
 politicalDiversity(US2004, index= "laakso/taagepera")

 pause()
 # 2014 Brazilian election outcome
seats_2014 = c(70, 66, 55, 37, 38, 34, 34, 26, 22, 20, 19, 15, 12, 11, 10, 9, 8, 5, 4, 3, 3, 3, 2, 2, 2, 1, 1, 1)

politicalDiversity(seats_2014, index= "laakso/taagepera")



