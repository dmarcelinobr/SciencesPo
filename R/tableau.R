#' @encoding UTF-8
#' @title Proportion Table for Univariate Analysis
#'
#' @description Produces a latex-like table of the variable proportions.
#'
#' @param var is the vector or the object to be computed proportions.
#' @param title A character vector as title.
#' @return A data frame object.
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#'
#' @keywords Descriptive
#' @keywords Tables 
#'
#' @examples
#'
#' x <- rep(1:5, 100)
#'
#' cap <- "My Table With Proportions"
#'
#' tableau(x, title = cap)
#'
#' @importFrom xtable xtable
#'
#' @export
#'
tableau <-
function(var, title = NULL){
        counts <- table(var)
        percts <- 100 * prop.table(counts)       
        
        print(
            xtable(
                cbind(
                    Count = counts
                    , Percent = percts
                )
                , caption = title
                , digits = c(0,0,2)
            )
            , caption.placement="top"
        )
    }
