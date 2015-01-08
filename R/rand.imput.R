#' @title Random Imputation 
#' 
#' @description Indeed a very simple but somewhat limited approach is to impute missing values from observed chosen uniformly randomly with replacement (this function is designed for teaching purposes). It assumes that \deqn{p(R|Z_{obs}, Z_{mis}) = p(R|\phi)} (MCAR). Sampling with replacement is important since it continues to favor values with higher incidence (preserving the MCAR empirical distribution).
#' 
#' @param x a vector with missing values \code{NA} to be imputed. 
#'
#' @details May also be combined with apply for matrix imputation drills.
#' 
#' @examples
#'   X <- c(1,2,NA,4,5,NA)
#' rand.imput(X)
#' @export
rand.imput <- function(x)  {
      gone <- is.na(x)
      there <- x[!gone]
      x[gone] <- sample(x=there,size=sum(gone),replace=TRUE)
      return(x)
  }