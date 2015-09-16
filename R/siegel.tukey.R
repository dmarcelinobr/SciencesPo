#' @title Siegel-Tukey Test for equality in variability
#'
#' @description Non-parametric Siegel-Tukey test for equality in variability.
#'  The null hypothesis is that the variability of x is equal between two groups.
#'  Thus, a rejection of the null hypothesis indicates that variability differs between the two groups.
#'  SiegelTukeyRank returns the ranks, calculated after Siegel Tukey logic.
#'
#' @param x A numeric vector of data values. Non-finite (e.g. infinite or missing) values will be omitted.
#' @param y A numeric vector of data values. Non-finite (e.g. infinite or missing) values will be omitted.
#'  @param id.col
#' @param adjust.median Should between-group differences in medians be leveled before performing the test? In certain cases, the Siegel-Tukey test is susceptible to median differences and may indicate significant differences in variability that, in reality, stem from differences in medians.
#' @param rnd
#' @param alternative A character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less". You can specify just the initial letter.
#' @param mu A number specifying an optional parameter used to form the null hypothesis.
#' @param paired
#' @param exact A logical indicating whether an exact p-value should be computed. This is passed directly to wilcox.test.
#' @param correct A logical indicating whether to apply continuity correction in the normal approximation for the p-value.
#' @param conf.int A logical indicating whether a confidence interval should be computed.
#' @param conf.level The confidence level of the interval.
#'
#' @references
#' Siegel, S., Tukey, J. W. (1960): A nonparametric sum of ranks procedure for relative spread in unpaired samples. Journal of the American Statistical Association.
#'
#' @examples
#' sample1 <- c(20.30, 22.53, 25.70, 13.23, 29.67, 24.46, 26.07, 19.35, 17.813,
#'             16.00, 15.30, 32.90)
#' sample2 <- c(10.56, 28.13, 19.94, 11.03, 8.093, 12.95, 21.14, 32.50, 10.90)
#' ks.test(sample1,sample2) # <-- Fail to reject H0 for alpha = .05
#' ks.test(sample1,sample2,alternative = "greater")
#' ks.test(sample1,sample2,alternative = "less")
#' @examples
#' x=c(4,4,5,5,6,6); y=c(0,0,1,9,10,10);
#'
#' siegel.tukey(x,y)
#'
#' @export
`siegel.tukey` <- function(x, y, id.col=FALSE, adjust.median=F,rnd=-1,alternative="two.sided",mu=0,paired=FALSE,exact=FALSE,correct=TRUE,conf.int=FALSE,conf.level=0.95)
{
  if(id.col==FALSE){
    data=data.frame(c(x,y),rep(c(1,2),c(length(x),length(y))))
  } else {
    data=data.frame(x,y)
  }
  names(data)=c("x","y")
  data=data[order(data$x),]
  if(rnd>-1){data$x=round(data$x,rnd)}

  if(adjust.median==T){
    data$x[data$y==1]=data$x[data$y==1]-(median(data$x[data$y==1])-median(data$x[data$y==2]))/2
    data$x[data$y==2]=data$x[data$y==2]-(median(data$x[data$y==2])-median(data$x[data$y==1]))/2
  }
  cat("Median of group 1 = ",median(data$x[data$y==1]),"\n")
  cat("Median of group 2 = ",median(data$x[data$y==2]),"\n","\n")
  cat("Test of median differences","\n")
  print(wilcox.test(data$x[data$y==1],data$x[data$y==2]))

  a=rep(seq(ceiling(length(data$x)/4)),each=2)
  b=rep(c(0,1),ceiling(length(data$x)/4))
  rk.up=c(1,(a*4+b))[1:ceiling(length(data$x)/2)]
  rk.down=rev(c(a*4+b-2)[1:floor(length(data$x)/2)])

  cat("Performing Siegel-Tukey rank transformation...","\n","\n")

  rks=c(rk.up,rk.down)
  unqs=unique(sort(data$x))
  corr.rks=tapply(rks,data$x,mean)
  cbind(unqs,corr.rks)
  rks.data=data.frame(unqs,corr.rks)
  names(rks.data)=c("unique values of x","tie-adjusted Siegel-Tukey rank")
  print(rks.data,row.names=F)
  names(rks.data)=c("unqs","corr.rks")
  data=merge(data,rks.data,by.x="x",by.y="unqs")

  rk1=data$corr.rks[data$y==1]
  rk2=data$corr.rks[data$y==2]
  cat("\n","Tie-adjusted Siegel-Tukey ranks of group 1","\n")
  group1=data.frame(data$x[data$y==1],rk1)
  names(group1)=c("x","rank")
  print(group1,row.names=F)
  cat("\n","Tie-adjusted Siegel-Tukey ranks of group 2","\n")
  group2=data.frame(data$x[data$y==2],rk2)
  names(group2)=c("x","rank")
  print(group2,row.names=F)
  cat("\n")

  cat("Siegel-Tukey test","\n")
  cat("Siegel-Tukey rank transformation performed.","Tie adjusted ranks computed.","\n")
  if(adjust.median==T) {cat("Medians adjusted to equality.","\n")} else {cat("Medians not adjusted.","\n")}
  cat("Rank sum of group 1 =", sum(rk1),"    Rank sum of group 2 =",sum(rk2),"\n")
  print(wilcox.test(rk1,rk2,alternative=alternative,mu=mu,paired=paired,exact=exact,correct=correct,conf.int=conf.int,conf.level=conf.level))
}
