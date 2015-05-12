# Pipe for element extraction
mtcars %>% (mpg)

# Pipe for element evaluation
  mtcars %>% (mpg) %>% ci()

# Pipe for evaluation and printing
 mtcars %>%
 "My regression estimates" %>%
  lm(formula = mpg ~ wt + cyl) %>%
  summary %>%
  coef

mtcars %>%
  "My subset table" %>%
  subset(mpg>=30)

# Pipe to an expression enclosed by parentheses with
# lambda expression in the form of x ~ f(x).
rnorm(100) %>% (x ~ plot(x,col="red",main=length(x)))

# Get information from datasets
data(titanic)
info(titanic)
# Show Observations Randomly Drawn from Data
peek(titanic)

# Parallel sum
data(us2012)
psum(us2012$Obama, us2012$Romney)

# Return Elapsed Time in Years
elapsed(from="1988-12-19", to="2014-12-31", format="%Y-%m-%d");
elapsed("1jan1960", "2jan1990", "%d%b%Y")
# Tables
## Express table entries as percentage of marginal table.
Aye <- sample(c("Yes", "Si", "Oui"), 177, replace = TRUE)
Bee <- sample(c("Hum", "Buzz"), 177, replace = TRUE)
( A <- table(Aye, Bee) )
perc.table(A)

## Crosstable
data(titanic)
crosstab(titanic, row.vars = "AGE", col.vars="SURVIVED", type = "t", style = "long", margins = T)

Express table entries as percentage of marginal table.
# Maximizes a Gaussian Distribution
maxGaussian(10, 4, 10)


# Generate Markov Chains
B<-matrix(rep(.3,16), nrow=4); diag(B)<-.1;
mc(B, 10)

# Plot Markov Chains
B<-matrix(rep(.3,16), nrow=4); diag(B)<-.1;
plotMC(100, B)

# Multiple ggplot objects
data(diamonds)
plot1 <- qplot(price,data=diamonds,binwidth=1000)
plot2 <- qplot(carat,price,data=diamonds)
multiPlot(plot1, plot2, ncols=2)

# Add footnotes
addFootnote(x = "My footnote", size = .9, color = "red")

# Plot of a normal density distribution with shaded areas
shadenorm(mu=2, sigma=10, outside=c(-3, 12), dens=15)
shadenorm(mu=2, sigma=15, between=c(-3, 12),lines=TRUE, col="blue",dens=15)

# Statistical Power
alt = seq(4,8, by=0.01);
pow = power(alt, mu=6, var=12, n=100)
plot(alt, pow, type="l", ylim=c(0,1), main="Power Plot")

# Calculates the effect size
omegaFactorial(8,2,3,169,3332,1978,3488);

# Distributions
## Binomial cumulative distribution function
trials = 10
prob = c(.2,.25,.3,.35)
success = 4
binompdf(n = trials, p = prob, x = success)
## Binomial probability density function
binomcdf(n = trials, p = prob, x = success)
## Inverse Cumulative Standard Normal Distribution
invnormal(area=0.35,mu=0,sigma=1)
## Normal probability density function
normalpdf(x=1.2,mu=0,sigma=1)
## Normal Cumulative Distribution
normalcdf(lower=-1.96,upper=1.96,mu=0,sigma=1)
##  Dirichlet distribution
rdirichlet(20, c(1,1,1) )
## Dirichlet distribution
ddirichlet(x, mat);


# Tests
## Association tests for contingency tables
x = sample(1:2, 30, TRUE);
y = sample(1:3, 30, TRUE);
a.test(table(x, y))

## Geary's test for normality
s <-sample(100, 20);
geary.test(s)

##  Log Likelihood of a Normal Distribution
 x = rnorm(100, 3, 7);
 logLik(x,3,7)

## Likelihood Ratio Test (LRT)
data(titanic);
mod0 <- glm( SURVIVED ~ CLASS, family=binomial, data=titanic);
mod1 <- glm( SURVIVED ~ CLASS + SEX, family=binomial, data=titanic);
lrtest(mod0, mod1)

## Resamples using the jackknife method
x = runif(10, 0, 1)
mean(x)
jackknife(x,mean)

#  Create Block-randomized Designs
(blk <- rand.block(blocksize = 6, n = 30, seed = 51));
table(blk$block, blk$condition)

# Random Imputation
x <- c(1,2,NA,4,5,NA)
rand.imput(x)

# Make Data Anonymous
data(ssex)
anonymize(ssex, keep.names=FALSE)

# Functions for teaching linear algebra
a <- c(1,0,0); b <- c(1,2,3); c <- c(4,5,6); x <- rnorm(3)
mat(~a+b)
mat(~a+b+1)


da <- expand.grid(var=gl(2,1), vin=gl(2,1), alu=gl(4,1), r=1:6);
da$ue <- with(da, interaction(var, vin, r));
da$y <- rnorm(nrow(da));



plot(1:(length(1800:2000)+1), type = "n", bty = "n", xaxt = "n", xlab = "", ylab = "", family = "Helvetica", ylim = c(0,1), cex.axis=1.2)
axis(1, at = (1:length(1800:2000))[1800:2000 %in% seq(1800,2000,10)], labels = seq(1800,2000,10), las = 2, lwd.tick = 1, family = "Helvetica", cex.axis = 1.2)

lines(sapply(all.S, mean)[sort.years %in% 1800:2000], type="l", col="grey60", lty=1, lwd=2)
lines(sapply(all.V, mean)[sort.years %in% 1800:2000], type="l", lty=1, lwd=2)

legend("topright",c("Saturation","Value"), col=c("grey60", "black"), lty=c(1, 1), bty="n", pch=16)

