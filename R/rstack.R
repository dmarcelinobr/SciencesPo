#' @title Stacked Table With Correlations
#' 
#' @description Pearson's product-moment coefficient is the well known degree to which two or more variables are linearly associated. In a two-dimensional table, the degree of correlation among the variables is quantified by the correlation coefficient.
#' 
#' @param m is a square matrix
#' 
#' @details The function generates a flatten table of correlations. If necessary, typing \sQuote{cor(t(x))} will coerce \sQuote{x} properly to a square matrix format. 
#' 
#' @return An table or a data.frame containing correlations for \code{m}
#' 
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' 
#' @note When using the function to correlate values of a variable with corresponding values at a different time will lead to an autocorrelation table.
#' 
#' @references Spiegel, M. R. (1992) Correlation Theory. in: \emph{Theory and Problems of Probability and Statistics,} 2nd ed. New York: McGraw-Hill, pp. 294--323.
#' 
#' @seealso \code{\link{rprob}}
#' 
#' @examples
#' data(nerlove63)
#' (table <- rprob(nerlove63) )
#' rstack(table)
#' 
#' @keywords Tables
#' @keywords Descriptive
#' 
#' @export
rstack <-
function(m) {
        if( (class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.") 
        if(!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
        ut <- upper.tri(m)
        data.frame(i = rownames(m)[row(m)[ut]],
                   j = rownames(m)[col(m)[ut]],
                   cor=t(m)[ut],
                   p=m[ut])
    }
