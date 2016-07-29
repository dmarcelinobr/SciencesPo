#' @encoding UTF-8
#' @title Then Operator
#' @description Then is a chain operator.
#' @name %>%
#' @export %>%
#' @keywords  internal
#' @rdname then
#' @usage x %>% f(y) is translated into f(x, y).
`%>%` <- magrittr::`%>%`



#' @encoding UTF-8
#' @title Not in (Find Matching or Non-Matching Elements)
#'
#' @description "%nin%" is a binary operator, which returns a
#'  logical vector indicating if there is a match or not for its left
#'  operand. A true vector element indicates no match in left operand,
#'  false indicates a match.
#'
#' @param x A vector of numeric, character, or factor values.
#' @param table	A vector (numeric, character, factor), matching the mode of x
#'
#' @examples
#' c('a','b','c') %nin% c('a','b')
#' @rdname nin
#' @export
"%nin%" <- function(x, table){
  match(x, table, nomatch = 0) == 0}
NULL


"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}
NULL




#' @encoding UTF-8
#' @title Arithmetic and Statistiscal Functions but Dealing with Missing Data
#'
#' @description Arithmetic and statistiscal functions but dealing with missing data by default.
#' @param x a numeric vector.
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @keywords internal
#' @examples
#' x <- c(NA,1,2,3,2,1,NA)
#'
#' Mean(x) #
#'
#' Sum(x) #
#'
#' Sums(x) #
#'
#' Mode(x) #
#'
#' sd(x, na.rm=TRUE)/sqrt(length(x))
#'
#' SE(x) #
#'
#' SD(x) #
#'
#' CV(x) #
#'
#' AAD(x) # Average Absolute Deviation
#'
#' @name Operations
NULL


#' @rdname Operations
#' @export
`Mean` <- function(x) as.numeric(mean(x, na.rm=TRUE))

#' @rdname Operations
#' @export
`Median` <- function(x) as.numeric(stats::median(x, na.rm=TRUE))


#' @rdname Operations
#' @export
`SD` <- function(x) as.numeric(stats::sd(x, na.rm=TRUE))


#' @rdname Operations
#' @export
`SE` <- function(x) {
  if (!is.numeric(x) && !is.complex(x) && !is.logical(x) && !is.vector(x)) stop ("The argument should be a numeric vector.")
  NAout <- x[!is.na(x)]
  ans <- sqrt(stats::var(NAout)/length(NAout))
  return(ans)
}
NULL


#' @rdname Operations
#' @export
`Sum` <- function(x) sum(x, na.rm=TRUE)



#' @rdname Operations
#' @export
`Sums` <- function(x) {

  if (sum(is.na(x))==length(x)) {
    y <- NA
  }

  else {y <- sum(x, na.rm=TRUE)}

  return(as.numeric(y))
}
NULL


#' @rdname Operations
#' @export
`Max`  <- function(x) {
  if (class(x) == "integer") {
    ifelse(all(is.na(x)), as.integer(NA) , max(x, na.rm = TRUE))
  }
  else {
    ifelse(all(is.na(x)), as.numeric(NA) , max(x, na.rm = TRUE))
  }
}
NULL

#' @rdname Operations
#' @export
`Min`  <- function(x) {
  if (class(x) == "integer") {
    ifelse(all(is.na(x)), as.integer(NA) , min(x, na.rm = TRUE))
  }
  else {
    ifelse(all(is.na(x)), as.numeric(NA) , min(x, na.rm = TRUE))
  }
}
NULL


#' @rdname Operations
#' @export
`CV` <- function(x) {
  ans = (SD(x) / Mean(x))
  return(ans)
}
NULL



#' @rdname Operations
#' @export
`Mode` <- function(x) {
   x = subset(x,!is.na(x))
   y <- as.factor(x)
  freqs <- summary(y)
  ans <- names(freqs)[freqs[names(freqs)] == max(freqs)]
  return(as.numeric(ans))
}
NULL



#' @rdname Operations
#' @export
`AAD` <- function(x, na.rm = TRUE, ...) {
  if (!is(x, "numeric") & !is(x, "integer")) {
    stop("\"x\" must be numeric")
  }
  if (!is(na.rm, "logical") | length(na.rm) != 1) {
    stop("\"na.rm\" must be a single logical value")
  }
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  ans <- mean(abs(x - mean(x)))
  return(ans)

}## -- end of AverageAbsoluteDeviation
NULL





#' @title Weighted Variance
#'
#' @description Compute the weighted variance.
#'
#' @param x a numeric vector.
#' @param weights a numeric vector of weights for \code{x}.
#' @param na.rm a logical if NA should be disregarded.
#' @keywords Exploratory
#' @export
#' @examples
#' wt=c(1.23, 2.12, 1.23, 0.32, 1.53, 0.59, 0.94, 0.94, 0.84, 0.73)
#' x = c(5, 5, 4, 4, 3, 4, 3, 2, 2, 1)
#'
#' WeightedVariance(x, wt)
#'
`WeightedVariance` <- function(x, weights = NULL, na.rm = FALSE) {
  if (na.rm) {
    weights <- weights[i <- !is.na(x)]
    x <- x[i]
  }
  sum.w <- sum(weights)
  sum.w2 <- sum(weights^2)
  mean.w <- sum(x * weights) / sum(weights)
  (sum.w / (sum.w^2 - sum.w2)) * sum(weights * (x - mean.w)^2, na.rm = na.rm)
}
NULL





#' @title Computes Weighted Mean
#' @description Compute the weighted mean of data.
#' @param x a numeric vector.
#' @param weights a numeric vector of weights of \code{x}.
#' @param normwt ignored at the moment.
#' @param na.rm a logical, if \code{TRUE}, missing data will be dropped.  If \code{na.rm = FALSE}, missing data will return an error.
#' @export
#' @examples
#' x <- sample(10,10)
#' w <- sample(5,10, replace=TRUE)
#'
#' WeightedMean(x, w)
`WeightedMean` <- function (x, weights = NULL, normwt = "ignored", na.rm = TRUE)
{
  if (!length(weights))
    return(mean(x, na.rm = na.rm))
  if (na.rm) {
    s <- !is.na(x + weights)
    x <- x[s]
    weights <- weights[s]
  }
  sum(weights * x)/sum(weights)
}
NULL




#' @title Computes Weighted Standardized Values
#' @description Computes weighted standardized values of data.
#' @param x a vector for which a set of standardized values is desired.
#' @param weights a vector of weights to be used to determining weighted values of \code{x}.
#'
#' @examples
#'  x <- sample(10,10)
#'  w <- sample(5,10, replace=TRUE)
#'
#' WeightedStdz(x, w)
#' @export
`WeightedStdz` <- function(x, weights=NULL){
  if(is.null(weights)){
    weights <- rep(1, length(x))
  }
  x <- (x - WeightedMean(x, weights, na.rm=TRUE))
  x <- (x/sqrt(WeightedVariance(x, weights, na.rm=TRUE)))
  return(x)
}
NULL


