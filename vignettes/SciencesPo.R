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

## ----eval=FALSE, echo=FALSE, message=FALSE, comment=NA-------------------
#  #' The function mcnemar.test can conduct McNemar’s test for matched pairs. For ex- ample, for Table 11.1,
#  ratings <- matrix(c(175, 16, 54, 188), ncol=2, byrow=TRUE,
#                    dimnames = list("2004 Election" = c("Democrat", "Republican"), "2008 Election" = c("Democrat", "Republican")))
#  mcnemar.test(ratings, correct=FALSE)

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

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
attach(stature)

Winsorize(winner.height)

# replacing 35 outlier elements we get same stature values: 
Winsorize(winner.height, k=35)

Winsorize(opponent.height, k=35)

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
attach(stature)

AAD(winner.height) 

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
data(religiosity)

Label(religiosity$Religiosity) <- "Religiosity index"

## ----echo=FALSE, message=FALSE, comment=NA-------------------------------


## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
require(SciencesPo)

table(iris$Species)

iris$Species <- Recode(iris$Species, "versicolor", 2)

table(iris$Species)

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
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

## ----eval=FALSE, echo=TRUE, message=FALSE, comment=NA--------------------
#  Destring(myvar)
#  
#  # Simulate some data (12 respondents x 4 items)
#  df <- data.frame(replicate(4, sample(1:5, 12, replace=TRUE)))
#  df <- data.frame(lapply(df, factor, ordered=TRUE,
#                            levels=1:5,
#                            labels=c("Strongly disagree","Disagree", "Neutral","Agree","Strongly Agree")))
#  grp <- gl(2, 12/2, labels=LETTERS[1:2]) # say equal group size for simplicity
#  

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
# Smithson and Verkuilen approach
(y = Normalize(x, method="SV"))

## ----echo=TRUE, message=FALSE--------------------------------------------
x <- as.double(c(0.1, 1, 10, 100, 1000, 10000))

Formatted(x) 

Formatted(x, "BRL")

p = c(0.25, 25, 50)

Formatted(p, "Perc", flag="+")

Formatted(p, "Perc", decimal.mark=",")

## ----echo=FALSE, message=FALSE, comment=NA-------------------------------

 dt <- data.frame(
 Z = sample(LETTERS,5),
 X = sample(1:5),
 Y = sample(c("yes", "no"), 5, replace = TRUE) )

dt;

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
dt %>% Anonymize()


## ----eval=FALSE, echo=TRUE, message=FALSE, comment=NA--------------------
#  normalpdf(x=1.2, mu=0, sigma=1)

## ----eval=FALSE, echo=TRUE, message=FALSE, comment=NA--------------------
#  normalcdf(lower=-1.96, upper=1.96, mu=0, sigma=1)

## ----eval=FALSE, echo=TRUE, message=FALSE, comment=NA--------------------
#  invnormal(area=0.35, mu=0, sigma=1)

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

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------

with(Quebec, Proportionality(pvotes, pseats, 
                     index = "Rae") )

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
with(Quebec, Proportionality(pvotes, pseats, 
                     index = "Loosemore-Hanby") )

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------

with(Quebec, Proportionality(pvotes, pseats, 
                     index = "Gallagher") )

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

## ----echo=TRUE, message=FALSE, fig.width=4.5, fig.height=4, fig.align="center", fig.cap= "2014 Legislative Election in Ceara (M=42)"----

mytable = HighestAverages(parties=names(Ceara), votes=Ceara, 
                seats = 42, method = "dh") 

p <- ggplot(mytable, aes(x=reorder(Party, Seats), y=Seats)) + 
  geom_lollipop() + coord_flip() + labs(x=NULL, y="# Seats")
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

PreviewTheme() + theme_grey()

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

## ----eval=FALSE, echo=FALSE, message=FALSE,  comment=NA, warning=FALSE----
#  
#  library(scales)
#  library(grid)
#  library(ggplot2)
#  library(plyr)
#  
#  df <- read.table(text = "year, district.id, party, seats, ideology
#  2012, 127, Stranka Pravde I Razvoja Bosne I Hercegovine, 1, p
#  2012, 127, SBB, 3, p
#  2008, 127, SDA, 13, p
#  2004, 127, SDA, 14, p
#  2008, 127, HDZ, 1, c
#  2008, 127, Stranka Pravde I Razvoja Bosne I Hercegovine, 1, p
#  2012, 127, SzBiH, 4, p
#  2000, 127, SDP, 8, m
#  2012, 127, NSRzB, 2, m
#  2012, 127, SDU, 1, p
#  2000, 127, SDA-SBiH, 15, p
#  2008, 127, SDP, 5, m
#  2008, 127, NSRzB, 1, m
#  2008, 127, LDS-SDU, 2, m
#  2000, 127, liberali-Bih-Gds-Bih, 1, m
#  2000, 127, NHI, 1, c
#  1997, 127, SDP, 3, m
#  2012, 127, SDP, 6, m
#  2004, 127, SzBiH, 5, p
#  1997, 127, BPS, 9, p
#  2000, 127, BPS, 3, p
#  2008, 127, SzBiH, 4, p
#  1997, 127, HDZ, 5, c
#  2000, 127, HDZ, 2, c
#  2012, 127, SDA, 10, p
#  2004, 127, SDP, 6, m
#  1997, 127, SDA-SBiH-Liberali-GDS, 13, p", sep=",", header = TRUE)
#  
#  df <- arrange(df, year, ideology, party)
#  
#  # conservative parties blue
#  cons <- ggplot(data = df[df$ideology == "c", ],
#                 aes(x = as.factor(year),
#                     y = seats,
#                     fill = party)) +
#    geom_bar(stat = "identity", position = "fill") +
#    scale_fill_manual(values = blue, name = "Conservative parties" )
#  
#  # progressive parties green
#  prog <- ggplot(data = df[df$ideology == "p", ],
#                 aes(x = as.factor(year),
#                     y = seats,
#                     fill = party)) +
#    geom_bar(stat = "identity", position = "fill") +
#    scale_fill_manual(values = green, name = "Progressive parties" )
#  
#  # moderate parties red
#  mod <- ggplot(data = df[df$ideology == "m", ],
#                aes(x = as.factor(year),
#                    y = seats,
#                    fill = party)) +
#    geom_bar(stat = "identity", position = "fill") +
#    scale_fill_manual(values = red, name = "Moderate parties" )

## ----echo=TRUE, message=FALSE, comment=NA, warning=FALSE-----------------
require(SciencesPo)

# default fontsize doesn't work well for online viewing
theme_set(theme_pub(base_size=18)) 

## ----echo=FALSE, message=FALSE, fig.align="center", fig.width=5, fig.height=3.5, comment=NA, warning=FALSE----
require(SciencesPo)

# default fontsize doesn't work well for online viewing
theme_set(theme_pub(base_size=18)) 

gg <- ggplot(mtcars, aes(mpg, disp,color=factor(carb),size=hp)) 
gg <- gg + geom_point(alpha=0.7) + labs(title="Bubble Plot")
gg <- gg +scale_size_continuous(range = c(3,8)) 
gg <- gg + theme_pub(legend = "none", base_size=18)
gg 

## ----echo=TRUE, message=FALSE, comment=NA, warning=FALSE-----------------
require(SciencesPo)

# "Verdana", "serif" and "sans" are also high-readability fonts
theme_set(theme_pub(base_size=18, base_family = "serif")) 

## ----echo=FALSE, message=FALSE, fig.align="center", fig.width=5, fig.height=3.5, comment=NA, warning=FALSE----
require(SciencesPo)


gg <- ggplot(mtcars, aes(mpg, disp,color=factor(carb),size=hp)) 
gg <- gg + geom_point(alpha=0.7) + labs(title="Bubble Plot")
gg <- gg +scale_size_continuous(range = c(3,8)) 
gg <- gg + theme_pub(legend = "none", base_size=18, base_family = "serif") 
gg

## ----echo=TRUE, message=FALSE, comment=NA, warning=FALSE-----------------

prefs <- theme(axis.text = element_text(size=18, 
                                        family = "serif", 
                                        colour="red"),
               legend.justification=c(1,0),
               legend.position=c(1,0), 
               legend.position="bottom")


## ----echo=FALSE, message=FALSE, fig.align="center", fig.width=5, fig.height=3.5, comment=NA, warning=FALSE----

prefs <- theme(axis.text = element_text(size=18, 
                                        family = "serif",
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

## ----echo=TRUE, fig.align="center", fig.width=5, fig.height=3.5, comment=NA, warning=FALSE----
theme_set(theme_pub(base_size=16))

gg <- ggplot(Presidents, aes(x=height_ratio, y=vote_support)) 
gg <- gg + geom_smooth(method=lm, colour="red", fill="gold")
gg <- gg + geom_point(size = 5, alpha = .7) 
gg <- gg +  xlim(0.9,1.2) + ylim(.40, .70)
gg <- gg + labs(x="Winner/Opponent height ratio", y="Winner vote share", title="Does height matter in presidential politics?")
gg <- gg + theme_pub()

# Commence adding layers here
Render(gg) + Footnote(note="danielmarcelino.github.io")

## ----eval=FALSE, echo=FALSE, fig.width=6, fig.height=4, message=FALSE, warning=FALSE, comment=NA----
#  library(SciencesPo)
#  library(extrafont)
#  # font_import()
#  exp_text <- "italic(y == frac(1, sqrt(2 * pi)) * e ^ {-frac(x^2, 2)} )"
#  
#  
#  library(ggplot2)
#  theme_set(theme_grey())
#  
#  set.seed(43121)
#  
#  means <- NULL
#  adj_means <- NULL
#  lambda <- 0.2
#  n <- 40
#  for(i in 1:1000){
#      vals <- rexp(n, rate=lambda)
#      means <- c(means, mean(vals))
#      adj_means <- c(adj_means, (1/lambda)+((mean(vals)-1/lambda)/sqrt(var(vals)/n)))
#  }
#  
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

## ----eval=FALSE, echo=FALSE, fig.width=6, fig.height=4, message=FALSE, warning=FALSE, comment=NA----
#  
#  d <- data.frame(x=c(1,1,2),y=c(1,2,2)*100)
#  
#  gg <- ggplot(d,aes(x,y))
#  gg <- gg + scale_x_continuous(expand=c(0.5,1))
#  gg <- gg + scale_y_continuous(expand=c(0.5,1))
#  gg + geom_spotlight(s_shape=1, expand=0) + geom_point(color="red")

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

