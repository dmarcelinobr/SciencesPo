## ----eval=FALSE, echo=TRUE, message=FALSE, comment=NA--------------------
#  install.packages("SciencesPo", dependencies = c("Depends", "Suggests"))

## ----eval=FALSE, echo=TRUE, message=FALSE, comment=NA--------------------
#  library("SciencesPo")
#  
#  ## Do things ...
#  
#  unloadNamespace("SciencesPo")

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
#  crosstable(titanic, SURVIVED)

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
Freq(titanic, SURVIVED) 

## ----eval=FALSE, echo=TRUE, message=FALSE, comment=NA--------------------
#  crosstable(titanic, SEX, CLASS)

## ----eval=FALSE, echo=TRUE, message=FALSE, comment=NA--------------------
#  crosstable(titanic, SEX, CLASS, column = FALSE)

## ----eval=FALSE, echo=TRUE, message=FALSE, comment=NA--------------------
#  crosstable(titanic, SEX, CLASS, SURVIVED)

## ----eval=FALSE, echo=TRUE, message=FALSE--------------------------------
#  require("SciencesPo")
#  
#  # Type 1:
#  skewness(pres, type = 1)
#  
#  kurtosis(pres, type = 1)
#  
#  # Type 2
#  skewness(pres, type = 2)
#  
#  kurtosis(pres, type = 2)
#  
#  # Type 3 is the default
#  skewness(pres)
#  
#  kurtosis(pres)

## ----eval=FALSE, echo=TRUE, message=FALSE--------------------------------
#  AAD(pres)
#  

## ----echo=TRUE, message=FALSE--------------------------------------------
Winsorize(pres)

## ----eval=FALSE, echo=TRUE, message=FALSE--------------------------------
#  data(alpha)
#  
#  str(alpha)
#  
#  # Note that a coefficient of .70 or higher is considered "acceptable" in most social science research situations.
#  
#  cronbach.alpha(alpha)
#  

## ----eval=FALSE, echo=TRUE, message=FALSE--------------------------------
#  
#  cor(alpha) # correlation
#  
#  factanal(alpha, factors = 1,
#           rotation="none") # varimax is the default
#  
#  scores="regression"
#  prcomp(alpha)

## ----echo=FALSE, message=FALSE-------------------------------------------

 dt <- data.frame(
 Z = sample(LETTERS,5),
 X = sample(1:5),
 Y = sample(c("yes", "no"), 5, replace = TRUE) )
dt;

dt %>% Anonymize()


## ----echo=FALSE, message=FALSE-------------------------------------------
require(SciencesPo)
str(iris)

iris_2 = Safechars(iris)

str(iris_2)

## ----echo=TRUE, message=FALSE--------------------------------------------
require(SciencesPo)

mylevels <- c('Strongly Disagree', 
              'Disagree', 
              'Neither', 
              'Agree', 
              'Strongly Agree')

myvar <- factor(sample(mylevels[1:5], 10, replace=TRUE))

## ----echo=TRUE, message=FALSE--------------------------------------------
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

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
alphas <- cbind(1:4, 1, 4:1);
rdirichlet(4, alphas );

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
# draw a sample from the posterior
set.seed(1234);
n <- 18116;
poll <- c(40,24,22,5,5,4) / 100 * n; # The data
mcmc <- 10000;
sim <- rdirichlet(mcmc, alpha = poll + 1);

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
# compute the margins: Aecio minus Marina
margins <- sim[,2] - sim[,3];

# What is the mean of the margins
# posterior mean estimate:
mean(margins); 

# posterior standard deviation:
sd(margins); 

# 90% credible interval:
quantile(margins, probs = c(0.025, 0.975)); 

# posterior probability of a positive margin (Aécio over Marina):
mean(margins > 0); 


## ----eval=FALSE, echo=TRUE, fig.width=4.5, fig.height=3.5, message=FALSE, comment=NA----
#  library(ggplot2)
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

## ----eval=FALSE, echo=TRUE, message=FALSE, comment=NA--------------------
#  trials = 10
#  prob = c(.2,.25,.3,.35)
#  success = 4
#  binompdf(n = trials, p = prob, x = success)

## ----eval=FALSE, echo=TRUE, message=FALSE, comment=NA--------------------
#  trials = 10
#  prob = c(.2,.25,.3,.35)
#  success = 4
#  binomcdf(n = trials, p = prob, x = success)

## ----politicalDiversity1, echo=TRUE, message=FALSE-----------------------
library("SciencesPo")

# The 1980 presidential election in the US (vote share):

(US1980 <- c("Democratic"=0.410, "Republican"=0.507,
              "Independent"=0.066, "Libertarian"=0.011,
              "Citizens"=0.003, "Others"=0.003));

PoliticalDiversity(US1980); # ENEP (laakso/taagepera) method 

PoliticalDiversity(US1980, index= "golosov");


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

## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
# politicalDiversity(Helsinki$votes); #ENEP Votes

PoliticalDiversity(Helsinki$seats.SL); #ENP for Saint-Lague

PoliticalDiversity(Helsinki$seats.dH); #ENP for D'Hondt

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

## ----echo=TRUE, message=FALSE, fig.width=4.5, fig.height=4.5, fig.align="center", fig.cap= "2014 Legislative Election in Ceara (M=42)"----

mytable = HighestAverages(parties=names(Ceara), votes=Ceara, 
                seats = 42, method = "dh") 

p <- ggplot(mytable, aes(x=reorder(Party, Seats), y=Seats)) + 
  geom_bar(position="dodge", stat = "identity") +
  coord_flip() + labs(x="", y="# Seats")
p + theme_grey() 

## ----eval=TRUE-----------------------------------------------------------
detach("package:SciencesPo")

ggplot(mtcars, aes(mpg, disp,color=factor(carb),size=hp)) + geom_point(alpha=0.7) + labs(title="Bubble Plot") + scale_size_continuous(range = c(3,10))

qplot(1:3, 1:3)

## ----eval=FALSE----------------------------------------------------------
#  set.seed(1)
#  test <- data.frame(
#    org = rep(c("Mammals", "Cats", "Tigers", "Lions", "Cheetahs"), 3),
#    level = rep(c("Animals", "Family", rep("Species", 3)), 3),
#    group = rep("Cats",15),
#    round = rep(c("Round1", "Round2", "Round3"),5),
#    obsAvg = runif(15, 1, 4)
#  )
#  
#  ggplot(test, aes(x = reorder(org, -as.numeric(level)), y = obsAvg, fill = level)) +
#    geom_bar(aes(alpha=round), stat = "identity", position = "dodge") +
#    scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
#    scale_alpha_manual(values = c(.5, .75, 1), guide = FALSE) +
#    labs(title = "Average Observation Score by by Round", y = "", x = "", fill = "Group") +
#    theme_538() +
#    geom_text(aes(label = round(obsAvg,1), group=round), vjust = -.5, size = 4, fontface="bold", position = position_dodge(width = .9)) +
#    scale_y_continuous(limits = c(0,4), expand = c(0,0)) +
#    theme(legend.position="bottom")
#  

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

## ----eval=TRUE-----------------------------------------------------------
require(SciencesPo)
qplot(1:3, 1:3)

## ----echo=FALSE, message=FALSE-------------------------------------------
require(SciencesPo)

theme_set(theme_pub(base_size=12)) # default fontsize doesn't work well for online viewing
qplot(1:3, 1:3)

## ----eval=FALSE, echo=FALSE, message=FALSE-------------------------------
#  require(SciencesPo)
#  # "Verdana", "serif" and "sans" are also high-readability fonts
#  theme_set(theme_pub(base_size=12, font_family = "Consolas"))
#  
#  qplot(1:3, 1:3)

## ----echo=FALSE, message=FALSE-------------------------------------------

prefs <- theme(axis.text = element_text(size=14, colour=NULL))

qplot(1:3, 1:3) + prefs

## ----echo=FALSE, message=FALSE-------------------------------------------
# Modifying a theme function
themeMod <- theme_gray() +
  theme(text = element_text(family = "Times", colour = "blue", size = 14))

ggplot(mpg, aes(x = cty, y = hwy, colour = factor(cyl))) + 
   geom_point(size = 2.5)

## ----echo=FALSE, message=FALSE-------------------------------------------
# Only change the 'colour' property of theme element 'text'

mytheme1 <- theme_grey() + theme(text = element_text(colour="red"))
mytheme1$text

## ----echo=FALSE, message=FALSE-------------------------------------------
# Replace the 'text' element entirely
mytheme2 <- theme_grey() %+replace% theme(text = element_text(colour="red"))
mytheme2$text

## ----eval=FALSE, message=FALSE-------------------------------------------
#  plot.mpg + background_grid(major = "xy", minor = "none")

## ----eval=FALSE, message=FALSE, fig.width=7, fig.height=5----------------
#  
#  plot.iris <- ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
#    geom_point() + facet_grid(. ~ Species) + stat_smooth(method = "lm") +
#    background_grid(major = 'y', minor = "none") + # add thin horizontal lines
#    panel_border() # and a border around each panel
#  # plot.mpg and plot.diamonds were defined earlier
#  ggdraw() +
#    draw_plot(plot.iris, 0, .5, 1, .5) +
#    draw_plot(plot.mpg, 0, 0, .5, .5) +
#    draw_plot_label(c("A", "B", "C"), c(0, 0, 0.5), c(1, 0.5, 0.5), size = 15)

## ----eval=FALSE, echo=FALSE, message=FALSE, fig.width=7, fig.height=5----
#  # Of course, we can also go crazy:
#  ggdraw() +
#    #geom_rect(data = boxes, aes(xmin = x, xmax = x + .15, ymin = y, ymax = y + .15),
#    #          colour = "gray60", fill = "red", alpha=.03) +
#    geom_path(data = spiral, aes(x = x, y = y, colour = t), size = 6, alpha = .4) +
#    draw_plot(plot.mpg, .3, .3, .4, .4) +
#    draw_plot(plot.iris, 0, .7, .7, .35 ) +
#    draw_plot(plot.iris, .45, .0, .6, .3 )

## ----echo=FALSE, message=FALSE, comment=NA, fig.height=3.5, fig.width=5, cache=TRUE----

theme_set(theme_pub())

data(presheights)
# Generating a ratio winner/opponent measure 
presheights = transform(presheights, 
                       height_ratio = winner.height/opponent.height) 


p <- ggplot(presheights, aes(x=height_ratio, y=winner.vote)) +
      geom_smooth(method=lm, colour="red", fill="gold")+
      geom_point(size = 5, alpha = .7) +
      xlim(0.85,1.2) + ylim(25, 70) +
      xlab("Winner/Opponent Height Ratio") + 
      ylab("Relative Support for the Winner")
p

## ----eval=FALSE, echo=TRUE, fig.height=3.5, fig.width=5,  message=FALSE, comment=NA----
#  theme_set(theme_pub())
#  # Avoiding missing data:
#  presheights <- subset(presheights, !is.na(height_ratio))
#  
#  fit=lm(winner.vote~height_ratio,data=presheights)
#  
#  mylabel=lm2eqn("Presidents","height_ratio","winner.vote")
#  
#  p + annotate(geom = 'text', x = 1.1, y = 70, size = 5,
#                 label = mylabel, fontface = 'italic')
#  
#  geom_foot("danielmarcelino.github.io", color = fade("brown1"),
#            rotn = -90, just ="right" )

## ----echo=TRUE, fig.width=4.5, fig.height=4.5----------------------------
library("scales", quietly = TRUE)

show_col(pub_color_pal("pub12")(12))

## ----echo=TRUE, fig.width=4.5, fig.height=3.5----------------------------
show_col(pub_color_pal("gray5")(6))

## ----echo=TRUE, fig.width=4.5, fig.height=4.5----------------------------
show_col(pub_color_pal("carnival")(5))

## ----echo=TRUE, fig.width=4.5, fig.height=4.5----------------------------
show_col(pub_color_pal("fte")(5))

## ----echo=TRUE, fig.width=5.5, fig.height=4.5----------------------------
show_col(pub_color_pal("manyeyes")(20))

## ----echo=TRUE, fig.width=4.5, fig.height=4.5----------------------------
show_col(pub_color_pal("tableau20")(5))

## ----echo=TRUE, fig.width=4.5, fig.height=4.5----------------------------
show_col(pub_color_pal("tableau10")(10))

## ----echo=TRUE, fig.width=4.5, fig.height=4.5----------------------------
show_col(pub_color_pal("tableau10medium")(10))

## ----echo=TRUE, fig.width=4.5, fig.height=4.5----------------------------
show_col(pub_color_pal("tableau10light")(10))

## ----echo=TRUE, fig.width=5.5, fig.height=4.5----------------------------
show_col(pub_color_pal("cyclic")(20))

## ----echo=TRUE, fig.width=4.5, fig.height=4.5----------------------------
show_col(pub_color_pal("purplegray12")(12))

## ----echo=TRUE, fig.width=4.5, fig.height=4.5----------------------------
show_col(pub_color_pal("greenorange12")(12))

## ----echo=TRUE, fig.width=4.5, fig.height=4.5----------------------------
show_col(pub_color_pal("bluered12")(12))

## ----echo=TRUE, fig.width=4.5, fig.height=4.5----------------------------
show_col(party_color_pal("BRA")(10))

## ----echo=TRUE, fig.width=4.5, fig.height=3.5----------------------------
show_col(pub_color_pal("trafficlight")(9))

## ----echo=TRUE, fig.width=4.5, fig.height=3.5----------------------------
show_col(pub_color_pal("bivariate1")(9))

## ----echo=TRUE, fig.width=4.5, fig.height=3.5----------------------------
show_col(pub_color_pal("bivariate2")(9))

## ----echo=TRUE, fig.width=4.5, fig.height=3.5----------------------------
show_col(pub_color_pal("bivariate3")(9))

## ----echo=TRUE, fig.width=4.5, fig.height=3.5----------------------------
show_col(pub_color_pal("bivariate4")(9))

## ---- echo=FALSE, message=FALSE, comment=NA------------------------------
print(sessionInfo())

