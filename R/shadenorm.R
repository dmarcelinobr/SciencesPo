#' @title Shades Normal Distribuion
#' 
#' @description Produces a plot of a normal density distribution with shaded areas.
#' 
#' @param below sets a lower endpoint.
#' @param above sets an upper endpoint.
#' @param pcts the 
#' @param mu the mean.
#' @param sig standard deviations.
#' @param numpts the number os points/observations to drawn upon. 
#' @param color the color of the area.
#' @param dens the density of the color.
#' @param justabove just plots the upper tail.
#' @param justbelow just plots the lower tail.
#' @param lines to draw lines.
#' @param between plots between specified points.
#' @param outside alternative "outside" area.
#' 
#' @return A plot with a normal distribution density with shaded areas
#'
#'@examples
#' shadenorm()
#' shadenorm(below=-1.5)
#' shadenorm(below=-1.5,justbelow=TRUE)
#' shadenorm(above=1.5, justabove=TRUE)
#' shadenorm(below=-1.5,above=1.5)
#' shadenorm(between=c(-4,0),color="black")
#' shadenorm(between=c(0,4),color="black")
#' shadenorm(between=c(-1,+1),color="darkgray")
#' title("P[-1 < z < 1] = 68%")
#' shadenorm(between=c(-2,+2),color="darkgray")
#' title("P[-2 < z < 2] = 95%")
#' shadenorm(between=c(-3,+3),color="darkgray")
#' title("P[-3 < z < 3] = 99.7%")
#' shadenorm(between = c(-1.75, 0, 2, 0.5, -1))  ## Plots between specified points
#' shadenorm(below=50,justbelow=TRUE,color="black",mu=47.3,sig=9.3)
#' 
#' ## Can plot one and then another on top of it using lines = TRUE
#' shadenorm(mu=2, sig=10, outside=c(-3, 12), dens=15)
#' shadenorm(mu=2, sig=15, between=c(-3, 12),lines=TRUE, col="blue",dens=15)
#' ## Example: Plotting a Hypothesis Test for the mean
#' ## Truth:      mu.true  = 8
#' ## Hypothesis: mu.ho    = 6
#' ## Generate Data Under Truth
#' mu.true = 5 ## Alternative Mean
#' mu.ho   = 6
#' sig     = 8
#' N       = 250 ## Sample Size
#' 
#' std.err = sig/sqrt(N)
#' crits = qnorm(c(0.025,0.975),mean=mu.ho, sd = std.err)
#' shadenorm(outside = crits, mu = mu.ho, sig = std.err,dens=15)
#' shadenorm(between = crits, mu = mu.true, sig = std.err, lines=TRUE, color="green",dens=15)
#' 
shadenorm <- function(below=NULL, above=NULL, pcts = c(0.025,0.975), mu=0, 
sig=1, numpts = 500, color = "gray", dens = 40, justabove= FALSE, justbelow = FALSE, lines=FALSE, between=NULL, outside=NULL) {
  
  if(is.null(between)){
    below = ifelse(is.null(below), qnorm(pcts[1],mu,sig), below)
    above = ifelse(is.null(above), qnorm(pcts[2],mu,sig), above)
  }
  
  if(is.null(outside)==FALSE){
    below = min(outside)
    above = max(outside)
  }
  lowlim = mu - 4*sig
  uplim  = mu + 4*sig
  
  x.grid = seq(lowlim,uplim, length= numpts)
  dens.all = dnorm(x.grid,mean=mu, sd = sig)
  if(lines==FALSE){
    plot(x.grid, dens.all, type="l", xlab="X", ylab="Density")
  }
  if(lines==TRUE){
    lines(x.grid,dens.all)
  }
  
  if(justabove==FALSE){
    x.below    = x.grid[x.grid<below]
    dens.below = dens.all[x.grid<below]
    polygon(c(x.below,rev(x.below)),c(rep(0,length(x.below)),rev(dens.below)),col=color,density=dens)
  }
  if(justbelow==FALSE){
    x.above    = x.grid[x.grid>above]
    dens.above = dens.all[x.grid>above]
    polygon(c(x.above,rev(x.above)),c(rep(0,length(x.above)),rev(dens.above)),col=color,density=dens)
  }
  
  if(is.null(between)==FALSE){
    from = min(between)
    to   = max(between)
    
    x.between    = x.grid[x.grid>from&x.grid<to]
    dens.between = dens.all[x.grid>from&x.grid<to]
    polygon(c(x.between,rev(x.between)),c(rep(0,length(x.between)),rev(dens.between)),col=color,density=dens)
  } 
}
