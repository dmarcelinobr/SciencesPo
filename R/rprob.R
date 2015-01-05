#' @title Pairwise Correlations With Probabilities 
#' 
#' @description Compute the pairwise correlations for all valid cases (!NA) in a data frame and find the probabilities for each combination. This function parses the correlations below the diagonal and the significance probabilities above it. 
#' 
#' @param x A data object
#' @param df An optional value for degrees of freedom. The default assumes you have a common degrees of freedom (n-2) for all correlations in the table.
#' 
#' @details The connexion between correlation and t-statistics is known to be \eqn{t = \frac{r \sqrt{(n-2)}} {\sqrt{(1-r^2)}}}. Or then, F = t^2 = r^2 * (n - 2)/(1 - r^2) ~ F(1, n-2). By solving this, it is possible to find the probabilities.
#' 
#' @return A data frame object with the Pearson's product-moment coefficient and its probabilitie. \bold{The first diagonal display the correlations, while the probabilities are shown in the second uppper diagonal}.
#' 
#' @references Aldrich, John (1995) Correlations Genuine and Spurious in Pearson and Yule. \emph{Statistical Science,} \bold{10(4),} 364--376.
#' @references Spiegel, M. R. (1992) Correlation Theory. in: \emph{Theory and Problems of Probability and Statistics,} 2nd ed. New York: McGraw-Hill, pp. 294--323.
#' 
#' @note You can print the whole matrix using \code{cor(t(x))}.
#' 
#' @seealso \code{\link{rstack}}
#' 
#' @examples
#' data(nerlove63)
#' 
#' rprob(nerlove63)
#' 
#' # a stacked up table 
#' rstack(rprob(nerlove63)) 
#' summary(lm(output ~ plabor + totcost, data = nerlove63))

#' # The final p-value of the OLS compares to the probabilities in the
#' # intersection of output and plabor and output and totcost in the matrix.
#' @keywords Tables
#' @keywords Descriptive                             
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' 
#' @export
rprob <-
function (x, df = nrow(x) - 2) {
        r <- cor(x, use="pairwise.complete.obs")
        table <- row(r) < col(r)
        r2 <- r[table]^2
        F <- r2 * df/(1 - r2)
        r[table] <- 1 - pf(F, 1, df)
        r[row(r) == col(r)] <- 1
        round(r, digits=2)
    }
