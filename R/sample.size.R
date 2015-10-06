#' @title N for Survey
#'
#' @param p The proportion.
#' @param delta The error size.
#' @param popsize An integer for the population size.
#' @param deff An intger for the deff.
#' @param alpha The level of alpha/significance.
#'
#' @export
#' @examples
#' # Comercial public opinion samples in Brazil:
#' sample.size(p=.50, delta=.03)
#' sample.size(p=.50, delta=.02)
`sample.size` <- function(p, delta = "auto", popsize=NULL, deff=1, alpha = .05){
  q <- 1-p
  pq <- cbind(p, q)
  minpq <- apply(pq, 1, min)
  if(any(delta=="auto")){
    delta <- ifelse(minpq >= .3, 0.1, ifelse(minpq >= .1, .05, minpq/2))
  }
  if(any(p >= 1) | any(delta >= 1) | any(popsize < 2) )
    stop("Proportion and delta both must < 1. Popsize must be >=2")
  else {
    n1 <- stats::qnorm(1-alpha/2)^2*p*(1-p)/delta^2
    if (!is.null(popsize)){
      n1 = n1/(1+n1/popsize)
    }
    if (deff != 1) {
      n1 = n1*deff }
  }
  deff1 <- deff
  if(deff==1) deff1 <- NULL
  table1 <- cbind(p, popsize, deff1, delta, round(n1))
  colnames(table1)[colnames(table1)=="deff1"] <- "deff"
  colnames(table1)[ncol(table1)] <- "n"
  returns <- list(p = p, delta=delta, popsize=popsize, deff=deff,
                  alpha = alpha, n1=n1, minpq=minpq,
                  table = as.data.frame(table1))
  class(returns) <- c("sample.size", "list")
  returns
}

### print sample.size
`print.sample.size` <- function(x, ...)
{
  if(nrow(x$table) < 6){
    cat("\n")
    cat("Sample size for survey.","\n")
    cat("Assumptions:", "\n")
    cat("  Proportion       =", x$p, "\n")
    cat("  Confidence limit =", round((1-x$alpha)*100), "%","\n")
    cat("  Delta            =", x$delta, "from the estimate.", "\n")
    if (!is.null(x$popsize)){
      cat("  Population size  =", x$popsize, "\n")
    }
    if (x$deff != 1) {
      cat("  Design effect    =", x$deff, "\n")
    }
    cat("\n")
    cat("  Sample size      =", round(x$n1), "\n")
    cat("\n")
  }else{
    cat("Sample size for surveys.","\n")
    cat("Assumptions:", "\n")
    if(length(x$alpha) == 1) cat("  Confidence limit =", round((1-x$alpha)*100), "%","\n")
    if(length(x$delta) == 1)	cat("  Delta            =", x$delta, "from the estimate.", "\n")
    print(x$table, rownames=FALSE)
  }
}
NULL
