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

## ----echo=TRUE, message=FALSE, cache=TRUE--------------------------------
require("SciencesPo")

data(stature)

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
attach(stature)

# Type 1:
Skewness(winner.height, type = 1)
# Type 2 
Skewness(winner.height, type = 2)
# Type 3, the default
Skewness(winner.height)

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
attach(stature)

# Type 1:
Kurtosis(winner.height, type = 1)
# Type 1:
Kurtosis(winner.height, type = 2)
# Type 3, the default
Kurtosis(winner.height)

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------

x <- sample (10, replace = TRUE)

Mode(x)

## ----echo=TRUE, message=FALSE, cache=TRUE, comment=NA--------------------
attach(stature)

CI(winner.height, level=.95) # confidence interval

CI(winner.height, level=.95)@mean # get only the mean 

CI(opponent.height, level=.95, na.rm = TRUE) # confidence interval

SE(winner.height) # std. error

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
attach(stature)

Winsorize(winner.height)

# replacing 35 outlier elements we get same stature values: 
Winsorize(winner.height, k=35)

Winsorize(opponent.height, k=35)

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
attach(stature)

AAD(winner.height) 

## ----echo=FALSE, cache=TRUE, message=FALSE, comment=NA-------------------
x <- sample(10)

# won't print normalized values by default, only to an object 
(y = Normalize(x) )

# equivalently to:
 (x-min(x))/(max(x)-min(x))

## ----echo=FALSE, message=FALSE, comment=NA-------------------------------
# Smithson and Verkuilen approach
(y = Normalize(x, method="SV"))

## ----echo=FALSE, message=FALSE, comment=NA-------------------------------

 dt <- data.frame(
 Z = sample(LETTERS,5),
 X = sample(1:5),
 Y = sample(c("yes", "no"), 5, replace = TRUE) )
dt;

dt %>% Anonymize()


## ----echo=FALSE, message=FALSE, comment=NA-------------------------------
require(SciencesPo)

table(iris$Species)

iris$Species <- Recode(iris$Species, "setosa", 1)

table(iris$Species)

## ----echo=FALSE, message=FALSE, comment=NA-------------------------------
require(SciencesPo)
str(iris)

iris_2 = Safechars(iris)

str(iris_2)

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
require(SciencesPo)

mylevels <- c('Strongly Disagree', 
              'Disagree', 
              'Neither', 
              'Agree', 
              'Strongly Agree')

# attibute some oredered factors 
myvar <- factor(sample(mylevels[1:5], 10, replace=TRUE))

unclass(myvar) # testing the order

## ----echo=TRUE, message=FALSE--------------------------------------------
Destring(myvar) 

## ----echo=TRUE, message=FALSE--------------------------------------------
x <- as.double(c(0.1, 1, 10, 100, 1000, 10000))

Formatted(x) 

Formatted(x, "BRL")

p = c(0.25, 25, 50)

Formatted(p, "Perc", flag="+")

Formatted(p, "Perc", decimal.mark=",")

## ----eval=FALSE, echo=TRUE, message=FALSE, comment=NA--------------------
#  normalpdf(x=1.2, mu=0, sigma=1)

## ----eval=FALSE, echo=TRUE, message=FALSE, comment=NA--------------------
#  normalcdf(lower=-1.96, upper=1.96, mu=0, sigma=1)

## ----eval=FALSE, echo=TRUE, message=FALSE, comment=NA--------------------
#  invnormal(area=0.35, mu=0, sigma=1)

## ----echo=TRUE, cache=TRUE, message=FALSE, comment=NA--------------------
alphas <- cbind(1:4, 1, 4:1);
rdirichlet(4, alphas );

## ----echo=TRUE, cache=TRUE, message=FALSE, comment=NA--------------------
# draw a sample from the posterior
set.seed(1234);
n <- 18116;
poll <- c(40,24,22,5,5,4) / 100 * n; # The data
mcmc <- 10000;
sims <- rdirichlet(mcmc, alpha = poll + 1);

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
Describe(sims)

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
# compute the margins: Aecio minus Marina
margins <- sims[,2] - sims[,3];

# What is the mean of the margins
# posterior mean estimate:
Mean(margins); 

# posterior standard deviation:
SD(margins); 

# 90% credible interval:
quantile(margins, probs = c(0.025, 0.975)); 

# posterior probability of a positive margin (Aécio over Marina):
Mean(margins > 0); 


## ----eval=FALSE, echo=TRUE, fig.width=4.5, fig.height=3.5, message=FALSE, comment=NA----
#  library(ggplot2)
#  
#  g <- ggplot(data=data.frame(means=adj_means), aes(x=means))
#  g <- g + ggtitle("Central Limit Theorem: Samples from Exponential Distribution")
#  g <- g + xlab("Means from 1000 Samples (n=40)") + ylab("Density")
#  g <- g + geom_histogram(
#      aes(y=..density..), fill="#400040", colour="#FFFFFF", binwidth=0.1
#  )
#  g <- g + geom_vline(
#      aes(xintercept=mean(means), colour="Actual Mean"), size=1,
#      show.legend=TRUE
#  )
#  g <- g + geom_vline(
#      aes(xintercept=1/lambda, colour="Expected Mean"), size=1,
#      show.legend=TRUE
#  )
#  g <- g + stat_function(fun=dnorm, args=list(mean=1/lambda),
#      aes(linetype="Normal Distribution"), colour="#D0D000", size=1,
#      show.legend=FALSE
#  )
#  g <- g + scale_colour_manual("Means", values=c(
#      "Expected Mean" = "#8080FF",
#      "Actual Mean" = "#FF8080",
#      "Normal Distribution" = NA
#  ))
#  g <- g + scale_linetype_manual("Functions", values=c(
#      "Expected Mean" = "blank",
#      "Actual Mean" = "blank",
#      "Normal Distribution" = "solid"
#  ))
#  g <- g + guides(
#      linetype = guide_legend(
#          override.aes = list(colour="#D0D000")
#      )
#  )
#  g
#  

## ----US-election, echo=FALSE, message=FALSE, comment=NA------------------

US1980 <- c("Democratic"=0.410, "Republican"=0.507,
              "Independent"=0.066, "Libertarian"=0.011,
              "Citizens"=0.003, "Others"=0.003);

## ----echo=FALSE, message=FALSE, comment=NA-------------------------------
US1980

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------

PoliticalDiversity(US1980, index = "laakso/taagepera");

PoliticalDiversity(US1980, index = "golosov");


## ----Helsinki-election, echo=FALSE, message=FALSE, comment=NA------------
# Helsinki's 1999

Helsinki <- data.frame(
  votes = c(68885, 18343, 86448, 21982, 51587,
            27227, 8482, 7250, 365, 2734, 1925,
            475, 1693, 693, 308, 980, 560, 590, 185),
  seats.SL=c(5, 1, 6, 1, 4, 2, 1, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0),
 seats.dH=c(5, 1, 7, 1, 4, 2, 0, 0, 0, 0, 0, 0, 0, 0,
           0, 0, 0, 0, 0))

print(Helsinki)

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
# ENEP defaults (laakso/taagepera) method 

with(Helsinki, PoliticalDiversity(votes)); #ENEP Votes

with(Helsinki, PoliticalDiversity(seats.SL)); #ENP for Saint-Lague

with(Helsinki, PoliticalDiversity(seats.dH)); #ENP for D'Hondt

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

Quebec

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------

with(Quebec, Proportionality(pvotes, pseats, 
                     index = "Rae") )

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
with(Quebec, Proportionality(pvotes, pseats, 
                     index = "Loosemore-Hanby") )

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------

with(Quebec, Proportionality(pvotes, pseats, 
                     index = "Gallagher") )

## ----Ceara-election, echo=TRUE, cache=TRUE-------------------------------
# Results for the state legislative house of Ceara (2014):
Ceara <- c("PCdoB"=187906, "PDT"=326841,"PEN"=132531, "PMDB"=981096,
           "PRB"=2043217,"PSB"=15061, "PSC"=103679, "PSTU"=109830,
           "PTdoB"=213988, "PTC"=67145, "PTN"=278267)

## ----highestAverages1, echo=TRUE, message=FALSE, comment=NA--------------
HighestAverages(parties=names(Ceara), votes=Ceara,
                seats = 42, method = "dh") 

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
HighestAverages(parties=names(Ceara), votes=Ceara,
                seats = 42, method = "sl") 

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
HighestAverages(parties=names(Ceara), votes=Ceara,
               seats = 42, method = "dh", threshold = 5/100) 

## ----eval=FALSE, echo=TRUE, message=FALSE, comment=NA--------------------
#  LargestRemainders(parties=names(Ceara), votes=Ceara,
#                  seats = 42, method = "hare")

## ----eval=FALSE, echo=TRUE, message=FALSE, comment=NA--------------------
#  LargestRemainders(parties=names(Ceara), votes=Ceara,
#                  seats = 42, method = "droop")

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
mytable = HighestAverages(parties=names(Ceara), votes=Ceara, 
                seats = 42, method = "dh") 

library(knitr)

kable(mytable, align=c("l","c","c"))

## ----echo=TRUE, message=FALSE, fig.width=3.5, fig.height=4, fig.align="center", fig.cap= "2014 Legislative Election in Ceara (M=42)"----

mytable = HighestAverages(parties=names(Ceara), votes=Ceara, 
                seats = 42, method = "dh") 

p <- ggplot(mytable, aes(x=reorder(Party, Seats), y=Seats)) + 
  geom_lollipop() + coord_flip() + labs(x="", y="# Seats")
p + theme_538() + theme(panel.grid.major.y = element_blank())

## ----echo=FALSE, message=FALSE, comment=NA-------------------------------
x <- c(778, 815, 857, 888, 925, 930, 965, 990, 1012)

# compute weight values
wgt <- runif(n=length(x))

# compute the lorenz with especific weights
Lorenz(x, wgt)

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
x <- c(778, 815, 857, 888, 925, 930, 965, 990, 1012)

wgt <- runif(n=length(x))

Gini(x, wgt)

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
x <- c(778, 815, 857, 888, 925, 930, 965, 990, 1012)

Atkinson(x, epsilon = 0.5)

## ----echo=TRUE, message=FALSE, comment=NA, fig.align="center", fig.width=5, fig.height=3.5----
# detach("package:SciencesPo", unload = TRUE)

gg <- ggplot(mtcars, aes(mpg, disp,color=factor(carb),size=hp)) 
gg <- gg + geom_point(alpha=0.7) + labs(title="Bubble Plot")
gg <- gg +scale_size_continuous(range = c(3,8)) 
gg 


## ----echo=TRUE, message=FALSE, comment=NA, fig.align="center", fig.width=5, fig.height=3.5----
library(SciencesPo)

gg <- ggplot(mtcars, aes(mpg, disp,color=factor(carb),size=hp)) 
gg <- gg + geom_point(alpha=0.7) + labs(title="Bubble Plot")
gg <- gg +scale_size_continuous(range = c(3,8)) 
gg <- gg + theme_blank()
gg

## ----echo=TRUE, message=FALSE, comment=NA, fig.align="center", fig.width=5, fig.height=3.5----
library(SciencesPo)

gg <- ggplot(mtcars, aes(mpg, disp,color=factor(carb),size=hp)) 
gg <- gg + geom_point(alpha=0.7) + labs(title="Bubble Plot")
gg <- gg +scale_size_continuous(range = c(3,8)) 
gg <- gg + theme_pub()
gg

## ----echo=TRUE, message=FALSE, comment=NA, fig.align="center", fig.width=5, fig.height=3.5----
library(SciencesPo)

gg <- ggplot(mtcars, aes(mpg, disp,color=factor(carb),size=hp)) 
gg <- gg + geom_point(alpha=0.7) + labs(title="Bubble Plot")
gg <- gg +scale_size_continuous(range = c(3,8)) 
gg <- gg + theme_538()
gg

## ----echo=TRUE, message=FALSE, comment=NA, fig.align="center", fig.width=5, fig.height=3.5----
library(SciencesPo)

gg <- ggplot(mtcars, aes(mpg, disp,color=factor(carb),size=hp)) 
gg <- gg + geom_point(alpha=0.7) + labs(title="Bubble Plot")
gg <- gg +scale_size_continuous(range = c(3,8)) 
gg <- gg + theme_darkside()
gg

## ----echo=FALSE, message=FALSE, comment=NA, fig.align="center", fig.width=6, fig.height=4----

set.seed(1)
test <- data.frame(
  org = rep(c("Coalition", "Opposition", "Right", "Center", "Left"), 3),
  level = rep(c("Government", "Opposition", rep("Parties", 3)), 3),
  group = rep("Opposition",15),
  election = rep(c("2006", "2010", "2014"),5),
  obsAvg = runif(15, 1, 4)
)

gg <- ggplot(test, aes(x = reorder(org, -as.numeric(level)), y = obsAvg, fill = level))
gg <- gg + geom_bar(aes(alpha=election), stat = "identity", position = "dodge")
gg <- gg + scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73"))
gg <- gg + scale_alpha_manual(values = c(.5, .75, 1), guide = FALSE)
gg <- gg + labs(title = "Average Score by Election", y = "", x = "", fill = "Group")
gg <- gg + geom_text(aes(label = round(obsAvg,1), group=election), vjust = -.3, size = 4, fontface="bold", position = position_dodge(width = .9))
gg <- gg + scale_y_continuous(limits = c(0,5), expand = c(0,0))
gg <- gg + theme_538(legend="bottom", base_size = 12)
gg

## ----eval=FALSE----------------------------------------------------------
#  municipality.plot <- ggplot(y, aes(x=as.factor(year), y=seats, fill=party, color=party)) +
#        geom_bar(bandwidth=1, stat="identity", group="party", position="fill") +
#        labs(x="year", y="% of seats for municipality")
#  
#    year district.id                                                         party seats ideology
#  1  2012         127                  Stranka Pravde I Razvoja Bosne I Hercegovine     1        p
#  2  2012         127                                Savez Za Bolju Buducnost (SBB)     3        p
#  3  2008         127                              Stranka Demokratske Akcije (SDA)    13        p
#  4  2004         127                              Stranka Demokratske Akcije (SDA)    14        p
#  5  2008         127                          Hrvatska Demokratska Zajednica (HDZ)     1        c
#  6  2008         127                  Stranka Pravde I Razvoja Bosne I Hercegovine     1        p
#  7  2012         127                        Stranka Za Bosnu I Hercegovinu (SzBiH)     4        p
#  8  2000         127                              Socijaldemokratska Partija (SDP)     8        m
#  9  2012         127                     Narodna Stranka Radom Za Boljitak (NSRzB)     2        m
#  10 2012         127                            Socijaldemokratska Unija Bih (SDU)     1        p
#  11 2000         127                                         Koalicija - SDA, SBiH    15        p
#  12 2008         127                              Socijaldemokratska Partija (SDP)     5        m
#  13 2008         127                     Narodna Stranka Radom Za Boljitak (NSRzB)     1        m
#  14 2008         127                                          Koalicija - LDS, SDU     2        m
#  15 2000         127 Lgk-liberalno-gradanska Koalicija Bih (liberali Bih, Gds Bih)     1        m
#  16 2000         127                               Nova Hrvatska Inicijativa (NHI)     1        c
#  17 1997         127                              Socijaldemokratska Partija (SDP)     3        m
#  18 2012         127                              Socijaldemokratska Partija (SDP)     6        m
#  19 2004         127                        Stranka Za Bosnu I Hercegovinu (SzBiH)     5        p
#  20 1997         127                 Bosanskohercegovacka Patriotska Stranka (BPS)     9        p
#  21 2000         127                 Bosanskohercegovacka Patriotska Stranka (BPS)     3        p
#  22 2008         127                        Stranka Za Bosnu I Hercegovinu (SzBiH)     4        p
#  23 1997         127                          Hrvatska Demokratska Zajednica (HDZ)     5        c
#  24 2000         127                          Hrvatska Demokratska Zajednica (HDZ)     2        c
#  25 2012         127                              Stranka Demokratske Akcije (SDA)    10        p
#  26 2004         127                              Socijaldemokratska Partija (SDP)     6        m
#  27 1997         127                          Koalicija - SDA, SBiH, Liberali, GDS    13        p
#  
#  
#  
#  #####################################3
#  # alt 2. Plot with separate legends for each ideology
#  
#  
#  # create separate plots for each ideology to get legends
#  
#  # conservative parties blue
#  cons <- ggplot(data = df2[df2$ideology == "c", ],
#                 aes(x = as.factor(year),
#                     y = seats,
#                     fill = party)) +
#    geom_bar(stat = "identity", position = "fill") +
#    scale_fill_manual(values = blue, name = "Conservative parties" )
#  
#  
#  # extract 'cons' legend
#  tmp <- ggplot_gtable(ggplot_build(cons))
#  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
#  legend_cons <- tmp$grobs[[leg]]
#  
#  
#  # progressive parties green
#  prog <- ggplot(data = df2[df2$ideology == "p", ],
#                 aes(x = as.factor(year),
#                     y = seats,
#                     fill = party)) +
#    geom_bar(stat = "identity", position = "fill") +
#    scale_fill_manual(values = green, name = "Progressive parties" )
#  
#  # extract 'prog' legend
#  tmp <- ggplot_gtable(ggplot_build(prog))
#  leg <- which(sapply(tmp$grobs, function(x) x$name) ==  "guide-box")
#  legend_prog <- tmp$grobs[[leg]]
#  
#  
#  # moderate parties red
#  mod <- ggplot(data = df2[df2$ideology == "m", ],
#                aes(x = as.factor(year),
#                    y = seats,
#                    fill = party)) +
#    geom_bar(stat = "identity", position = "fill") +
#    scale_fill_manual(values = red, name = "Moderate parties" )
#  
#  
#  # extract 'mod' legend
#  tmp <- ggplot_gtable(ggplot_build(mod))
#  leg <- which(sapply(tmp$grobs, function(x) x$name) ==  "guide-box")
#  legend_mod <- tmp$grobs[[leg]]
#  
#  #######################################
#  
#  
#  # arrange plot and legends
#  
#  # define plotting regions (viewports) for plot and legends
#  vp_plot <- viewport(x = 0.25, y = 0.5,
#                      width = 0.5, height = 1)
#  
#  vp_legend_cons <- viewport(x = 0.66, y = 0.87,
#                             width = 0.5, height = 0.15)
#  
#  vp_legend_prog <- viewport(x = 0.7, y = 0.55,
#                             width = 0.5, height = 0.60)
#  
#  vp_legend_mod <- viewport(x = 0.75, y = 0.2,
#                            width = 0.5, height = 0.30)
#  
#  # clear current device
#  grid.newpage()
#  
#  # add objects to the viewports
#  # plot without legend
#  print(g1 + theme(legend.position = "none"), vp = vp_plot)
#  upViewport(0)
#  
#  # legends
#  pushViewport(vp_legend_cons)
#  grid.draw(legend_cons)
#  upViewport(0)
#  
#  pushViewport(vp_legend_prog)
#  grid.draw(legend_prog)
#  upViewport(0)
#  
#  pushViewport(vp_legend_mod)
#  grid.draw(legend_mod)

## ----echo=TRUE, message=FALSE, comment=NA, warning=FALSE-----------------
require(SciencesPo)

# default fontsize doesn't work well for online viewing
theme_set(theme_pub(base_size=20)) 

## ----echo=FALSE, message=FALSE, fig.align="center", fig.width=5, fig.height=3.5, comment=NA, warning=FALSE----
require(SciencesPo)

# default fontsize doesn't work well for online viewing
theme_set(theme_pub(base_size=20)) 

gg <- ggplot(mtcars, aes(mpg, disp,color=factor(carb),size=hp)) 
gg <- gg + geom_point(alpha=0.7) + labs(title="Bubble Plot")
gg <- gg +scale_size_continuous(range = c(3,8)) 
gg <- gg + theme_pub(legend = "none", base_size=20)
gg 

## ----echo=TRUE, message=FALSE, comment=NA, warning=FALSE-----------------
require(SciencesPo)

# "Verdana", "serif" and "sans" are also high-readability fonts
theme_set(theme_pub(base_size=20, base_family = "sans")) 

## ----echo=FALSE, message=FALSE, fig.align="center", fig.width=5, fig.height=3.5, comment=NA, warning=FALSE----
require(SciencesPo)


gg <- ggplot(mtcars, aes(mpg, disp,color=factor(carb),size=hp)) 
gg <- gg + geom_point(alpha=0.7) + labs(title="Bubble Plot")
gg <- gg +scale_size_continuous(range = c(3,8)) 
gg <- gg + theme_pub(legend = "none", base_size=20, base_family = "sans") 
gg

## ----echo=TRUE, message=FALSE, comment=NA, warning=FALSE-----------------

prefs <- theme(axis.text = element_text(size=20, 
                                        colour="red"),
               legend.justification=c(1,0),
               legend.position=c(1,0), 
               legend.position="bottom")


## ----echo=FALSE, message=FALSE, fig.align="center", fig.width=5, fig.height=3.5, comment=NA, warning=FALSE----

prefs <- theme(axis.text = element_text(size=20, 
                                        colour="red"),
               legend.justification=c(1,0),
               legend.position=c(1,0), 
               legend.position="bottom")

gg <- ggplot(mtcars, aes(mpg, disp,color=factor(carb),size=hp)) 
gg <- gg + geom_point(alpha=0.7) + labs(title="Bubble Plot")
gg <- gg +scale_size_continuous(range = c(3,8)) 
gg <- gg + theme_pub(legend = "none")
gg <- gg + prefs
gg

## ----echo=TRUE, message=FALSE, comment=NA, ache=TRUE---------------------

theme_set(theme_pub(base_size = 12, base_family = "serif"))

data(stature)
# Generating a ratio winner/opponent measure 
heights = transform(stature, 
                       height_ratio = winner.height/opponent.height) 

gg <- ggplot(heights, aes(x=height_ratio, y=winner.vote)) +
      geom_smooth(method=lm, colour="red", fill="gold")+
      geom_point(size = 5, alpha = .7) +
      xlim(0.85,1.2) + ylim(25, 70) +
      xlab("Winner/Opponent Height Ratio") + 
      ylab("Relative Support for the Winner")
gg



## ----eval=FALSE, echo=TRUE, fig.align="center", fig.height=5, fig.width=7,  message=FALSE, comment=NA----
#  theme_set(theme_pub())
#  # Avoiding missing data:
#  heights <- subset(heights, !is.na(height_ratio))
#  
#  fit=lm(winner.vote~height_ratio,data=heights)
#  
#  mylabel=lm2eqn("Presidents","height_ratio","winner.vote")
#  
#  p + annotate(geom = 'text', x = 1.1, y = 70, size = 5,
#                 label = mylabel, fontface = 'italic')
#  
#  geom_foot("danielmarcelino.github.io", color = fade("brown1"),
#            rotn = -90, just ="right" )

## ----echo=TRUE,  fig.align="center", fig.width=4.5, fig.height=4, comment=NA, warning=FALSE----
library("scales", quietly = TRUE)

show_col(pub_color_pal("pub12")(12))

## ----echo=TRUE,  fig.align="center", fig.width=3.5, fig.height=3.5, comment=NA, warning=FALSE----
show_col(pub_color_pal("gray5")(6), labels = FALSE)

## ----echo=TRUE,  fig.align="center", fig.width=3.5, fig.height=3.5, comment=NA, warning=FALSE----
show_col(pub_color_pal("carnival")(6))

## ----echo=TRUE,  fig.align="center", fig.width=3.5, fig.height=3.5, comment=NA, warning=FALSE----
show_col(pub_color_pal("fte")(4))

## ----echo=TRUE,  fig.align="center", fig.width=6, fig.height=4.5, comment=NA, warning=FALSE----
show_col(pub_color_pal("manyeyes")(20))

## ----echo=TRUE,  fig.align="center", fig.width=6, fig.height=4.5, comment=NA, warning=FALSE----
show_col(pub_color_pal("tableau20")(20))

## ----echo=TRUE,  fig.align="center", fig.width=4.5, fig.height=4, comment=NA, warning=FALSE----
show_col(pub_color_pal("tableau10")(10))

## ----echo=TRUE,  fig.align="center", fig.width=4.5, fig.height=4, comment=NA, warning=FALSE----
show_col(pub_color_pal("tableau10medium")(10))

## ----echo=TRUE,  fig.align="center", fig.width=4.5, fig.height=4, comment=NA, warning=FALSE----
 show_col(pub_color_pal("tableau10light")(10))

## ----echo=TRUE,  fig.align="center", fig.width=6, fig.height=4.5, comment=NA, warning=FALSE----
show_col(pub_color_pal("cyclic")(20))

## ----echo=TRUE,  fig.align="center", fig.width=4.5, fig.height=4, comment=NA, warning=FALSE----
show_col(pub_color_pal("purplegray12")(12))


## ----echo=TRUE,  fig.align="center", fig.width=4.5, fig.height=4, comment=NA, warning=FALSE----
show_col(pub_color_pal("greenorange12")(12))

## ----echo=TRUE,  fig.align="center", fig.width=4.5, fig.height=4, comment=NA, warning=FALSE----
show_col(pub_color_pal("bluered12")(12))

## ----echo=TRUE, fig.align="center", fig.width=6, fig.height=5, comment=NA, warning=FALSE----
party_color_pal("BRA", plot=TRUE)

## ----echo=TRUE,  fig.align="center", fig.width=4.5, fig.height=3.5, comment=NA, warning=FALSE----
show_col(pub_color_pal("trafficlight")(9))

## ----echo=TRUE,  fig.align="center", fig.width=4.5, fig.height=3.5, comment=NA, warning=FALSE----
show_col(pub_color_pal("bivariate1")(9))

## ----echo=TRUE,  fig.align="center", fig.width=4.5, fig.height=3.5, comment=NA, warning=FALSE----
show_col(pub_color_pal("bivariate2")(9))

## ----echo=TRUE,  fig.align="center", fig.width=4.5, fig.height=3.5, comment=NA, warning=FALSE----
show_col(pub_color_pal("bivariate3")(9))

## ----echo=TRUE,  fig.align="center", fig.width=4.5, fig.height=3.5, comment=NA, warning=FALSE----
show_col(pub_color_pal("bivariate4")(9))

## ---- echo=FALSE, message=FALSE, comment=NA------------------------------
print(sessionInfo(), locale=FALSE)
# library(help='SciencesPo')

