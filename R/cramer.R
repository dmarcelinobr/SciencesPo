#' @encoding UTF-8
#' @title Cramer's V for a contingency table
#'
#' @description Produce the Cramér V / Phi test using two vectors with more than 2 levels.
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#'  
#' @param x one vector
#' @param y the other vector
#' 
#' @return The table's Cramer's V.
#' 
#' @details Compute Cramer's V for a resulting table with more than 2x2 fields.
#' 
#' @examples
#'  x =sample(1:2, 30, TRUE); 
#'  y= sample(1:3, 30, TRUE)
#'  cramer(x, y)
#'
#' @export
cramer <- function(x, y) {
	chisq_test <- function (x, y) {
	  O <- table(x, y)
	  n <- sum(O)

	  E <- outer(rowSums(O), colSums(O), "*")/n

	  sum((abs(O - E))^2 / E)
	}
  chi <- chisq_test(x, y)

  ulength_x <- length(unique(x))
  ulength_y <- length(unique(y))
# Cramér V / Phi:
  sqrt(chi / (length(x) * (min(ulength_x, ulength_y) - 1)))
}

#' # Cramer's V for a contingency table
#' # Compute Cramer's V for a table with more than 2x2 fields.
#' # @param tab a table or ftable object. Tables of class xtabs and other will be coerced to ftable internally.
#' # cramer <-
#' # function (tab) 
#' # {
#' #  if (class(tab) != "ftable") 
#' #    tab <- ftable(tab)
#' #  phi <- sjs.phi(tab)
#' #  cramer <- sqrt(phi^2/min(dim(tab) - 1))
#' #  return(cramer)
#' # }

#' #x <- vcd::Arthritis$Improved
#' #y <- vcd::Arthritis$Treatment
#' #correct <- vcd::assocstats(table(x, y))$cramer
#' #correct
#' # is_ok <- function(x) stopifnot(all.equal(x, correct))
#' 
#' # is_ok(cramer(x, y))
#' #
#' #microbenchmark(
#' #  cramer1(x, y),
#' #  cramer(x, y),
#' #  cramer_c(x, y)
#' #)

#' # cramer_c <- compiler::cmpfun(cramer)
