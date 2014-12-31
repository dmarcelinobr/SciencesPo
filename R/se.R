#' @title Calculate the Standard Error
#'
#' @description Compute the standard errors of a numeric vector
#' 
#' @aliases std.error
#' @param x  A vector of class numeric or integer
#' @param na.rm A logical value indicating whether \code{NA} should be stripped before  computating.
#' 
#' @details The standard error of the mean (SEM) (\emph{assuming statistical independence of the values in the sample}) is estimated by taking the standard deviation of the population sample, divided by the square root of the sample size: \deqn{se = \frac{{s}}{{\sqrt{n}}}}
#' 
#' @references Kenney, J. F. and Keeping, E. S. (1951) Standard Error of the Mean. In \emph{Mathematics of Statistics,} Pt. 2, 2nd ed. Princeton, NJ: Van Nostrand, pp. 110 and 132--133.
#' 
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#'
#' @examples
#' x <- rnorm(100); 
#' se(x)
#'
#' @export 
se <-
function (x, na.rm = TRUE) 
    {
        valid <- function(x) return(sum(!is.na(x)))
        dim <- dim(x)
        if (is.null(dim)) {
            sd <- sd(x, na.rm = na.rm)
            n.valid <- valid(x)
        }
        else {
            if (is.data.frame(x)) {
                n.valid <- unlist(sapply(x, valid))
                sd <- unlist(sapply(x, sd, na.rm = na.rm))
            }
            else {
                n.valid <- unlist(apply(x, 2, valid))
                sd <- unlist(apply(x, 2, sd, na.rm = na.rm))
            }
        }
        return(sd/sqrt(n.valid))
    }
