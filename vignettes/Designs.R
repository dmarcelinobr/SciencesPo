## ----echo=TRUE, message=FALSE, comment=NA--------------------------------
library("SciencesPo", quietly = TRUE)

## ----echo=TRUE, message=FALSE, comment=NA, cache=TRUE--------------------
blk <- rbd(blocksize = 20, n = 80, seed = 51)

head(blk)

## ----echo=TRUE, message=FALSE, comment=NA, cache=TRUE--------------------
print(blk);

## ----echo=TRUE, message=FALSE, comment=NA, cache=TRUE--------------------
crosstable(blk, block, condition)

## ----echo=TRUE, message=FALSE, comment=NA, cache=TRUE--------------------
 set.seed(51);
 blk$y <- rnorm(n = 80, mean = 20, sd = 5)

## ----echo=TRUE, message=FALSE, comment=NA, cache=TRUE--------------------
(mean = tapply(blk$y, list(blk$condition, blk$block), mean))
(sd = tapply(blk$y, list(blk$condition, blk$block), sd))

## ----echo=TRUE, message=FALSE, comment=NA, cache=TRUE--------------------
fit.aov <- aov(y ~ factor(condition) + factor(block), data=blk)
summary(fit.aov) # display Type I ANOVA table
drop1(fit.aov,~.,test="F") # type III SS and F Tests


## ----echo=TRUE, message=FALSE, comment=NA, cache=TRUE--------------------
model.tables(fit.aov, "means", se=TRUE) # SE for differences, NOT for means

## ----echo=TRUE, message=FALSE, comment=NA, cache=TRUE--------------------
pooled.se = sqrt(1688.1/4)

## ----echo=TRUE, message=FALSE, comment=NA, cache=TRUE--------------------
block <- c(1,2,3,4) # the values of the x axis
Control <- mean[1,] # the results from the means output
Treat <- mean[2,] # the results from the means output

## ----echo=TRUE, message=FALSE, comment=NA, cache=TRUE--------------------
plot(block, Treat, type = "b", ylab = "outcome", xlab = "blocks of experimental conditions", ylim = c(0, 30) )


## ----echo=TRUE, message=FALSE, comment=NA, cache=TRUE--------------------
fit.lm <- lm(y ~ factor(condition) + factor(block), data = blk)
anova(fit.aov)

