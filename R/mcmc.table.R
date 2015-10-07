#' Summarize MCMC output in a table-like
#'
#' The function produces a table with means, SDs, and 95% credible intervals
#' from MCMC output from the following packages: \code{R2jags}, \code{rjags}, \code{R2WinBUGS}, \code{R2OpenBUGS}, and \code{MCMCpack}.
#' @param obj The MCMC object.
#' @param ci The level for the confidence interval.
#' @export
mcmc.table <- function(obj, ci = 0.95)
{
  s1 <- Sys.time()
  if(class(obj) == "jags" | class(obj) == "rjags"){
    obj <- as.matrix(coda::as.mcmc(obj))
  }
  if(class(obj) == "bugs"){
    obj <- obj$obj.matrix
  }
  if(class(obj) == "mcmc"){
    obj <- as.matrix(obj)
  }
  if(class(obj) == "mcmc.list"){
    obj <- as.matrix(obj)
  }

  dat <- t(obj)
  ans <- apply(dat, 1,
                   function(x) c(Mean = round(mean(x), digits = 3), # Posterior mean
                                 SD = round(sd(x), digits = 3), # Posterior SD
                                 Lower = as.numeric(round(stats::quantile(x, probs = c((1 - ci) / 2)), digits = 3)), # Lower CI of posterior
                                 Upper = as.numeric(round(stats::quantile(x, probs = c((1 + ci) / 2)), digits = 3)), # Upper CI of posterior
                                 Pr = round(ifelse(mean(x) > 0, length(x[x > 0]) / length(x), length(x[x < 0]) / length(x)), digits = 3) # Probability of posterior >/< 0
))
  s2 <- Sys.time()
  timediff <- c( s2 - s1 )
  cat("\n")
  cat("Date of Analysis: ",format(Sys.time(), "%a %b %d %Y"), "\n", "Computation time: ",timediff,sep="","\n")
  cat("-----------------------------------\n")
  return(t(ans))
}
