#' Shades Normal Distribuion
#'
#'
#'
#'
#'
#'
#'@examples
#' shadenorm(between=c(-4,0),color="black")
#' shadenorm(between=c(0,4),color="black")
#' shadenorm(between=c(-1,+1),color="darkgray")
#' title("P[-1 < z < 1] = 68%")
#' shadenorm(between=c(-2,+2),color="darkgray")
#' title("P[-2 < z < 2] = 95%")
#' shadenorm(between=c(-3,+3),color="darkgray")
#' title("P[-3 < z < 3] = 99.7%")
#' shadenorm(below=50,justbelow=TRUE,color="black",mu=47.3,sig=9.3)

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
