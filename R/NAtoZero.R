#' @title Changes NAs in a vector into a given value
#'
#' @description Changes NAs in a vector into a given value
#' @param x the vector
#' @param value the value to be given to the missing value \code{NA}.
#' @seealso \link{randomImputation}.
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @examples
#' v <- sample(c(round(runif(5, 1, 3)), rep(NA, 2)))
#' NAtoZero(v)
#'
#'@export
NAtoZero <- function(x, value = 0){
    x[is.na(x) == TRUE] <- value
    return(x)
}
NULL

#' @title Eliminate all observations with at least one NA in a data frame
#'
#' @description Generates two matrices: One with complete observations and one with all observations containing at least one missing value.
#' @param Dataframe with observations in rows.
#' complete}{Dataframe containing complete observations.}
#' incomplete}{Dataframe containing observations with at least one \code{NA}.}
#'
#'\seealso{\code{\link{complete.cases}}}
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @examples
#' id <- 1:10; var1 <- rnorm(10); var2 <- rgamma(10, 2, 1);
#' df <- data.frame(cbind(id, var1, var2))
#' df[c(5, 9), 3] <- NA
#' eliminateNA(df)
#' 
#'@export
eliminateNA <- function(x){
  rows <- dim(x)[1]
  cols <- dim(x)[2]
  tmp <- matrix(NA, ncol = cols, nrow = rows)
  for (i in 1:cols){tmp[, i] <- as.numeric(x[, i])}
  compl <- x[complete.cases(x) == TRUE, ]
  incompl <- x[complete.cases(x) == FALSE, ]
  ans <- list(complete = compl, incomplete = incompl)
  return(ans)
}
