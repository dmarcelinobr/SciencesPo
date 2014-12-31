#' @title Method to Produce Descriptive Statistics Summary
#' @param ... Parameters which are typically ignored
#' @return A data.frame of descriptive statistics 
#' @export summarize
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
summarize <- function(...)
  UseMethod("summarize")


print.summarize<-function(x,...){
  cat("summarize: ")
  print(x$call) 
}
NULL

#' @title Default Summary Statistics Function
#' 
#' @description This function provides up to 14 statistics for an entire data object: number of cases, mean, standard deviation, variance, standard error, median, mad (median absolute deviation), trimmed and winsorized means, range, minimum, maximum, skewness, and kurtosis.
#' 
#' @details Trimming is not winsorizing. The winsorization process is more complex than simply excluding data. For example, while in a trimmed estimator the extreme values are discarded, in a winsorized estimator, they are rather replaced by certain percentiles.
#' 
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @param x A vector or a data frame.
#' @param by A factor variable 
#' @param basic A logical value indicating if a short version of the descriptive table might be returned
#' @param na.rm A logical value indicating whether NA values should be stripped before the computations
#' @param trim Is the proportion of the data to be replaced for estimating the average
#' @param type A numeric value (fraction) to be trimmed. The value in trim will be discarded from the top and bottom of data. See in details below 
#' @param k A numeric value for observations in the data set to be discarded while computing the winsorized mean. See details below
#' 
#' @return A data frame containing the require computations
#' 
#' @keywords Descriptive
#' @keywords Tables
#'
#' @references Venables, W. N. and Ripley, B. D. (2002) \emph{Modern Applied Statistics with S.}. Springer.
#' 
#' @examples
#' #load some data
#' data(ssex) 
#'
#' # To apply the function
#' summarize(ssex, trim = 0.5, k = 3)
#'
#' @export
summarize <-
  function (x, by = NULL, basic = FALSE, na.rm = TRUE, trim = 0.2, type = 2, k = 1)
  {
    cl <- match.call()
    valid <- function(x) {
      sum(!is.na(x))
    }
    if (!na.rm)
      x <- na.omit(x)
    
    if (is.null(dim(x)[2])) {
      len <- 1
      stats = matrix(rep(NA, 13), ncol = 13)
      stats[1, 1] <- valid(x)
      stats[1, 2] <- mean(x, na.rm = na.rm)
      stats[1, 3] <- sd(x, na.rm = na.rm)
      stats[1, 4] <- var(x, na.rm = na.rm)
      stats[1, 5] <- se(x, na.rm = na.rm)
      stats[1, 6] <- median(x, na.rm = na.rm)
      stats[1, 7] <- mad(x, na.rm = na.rm)
      stats[1, 8] <- mean(x, na.rm = na.rm, trim = trim)
      stats[1, 9] <- winsorize(x, k=k, na.rm = na.rm)
      stats[1, 10] <- min(x, na.rm = na.rm)
      stats[1, 11] <- max(x, na.rm = na.rm)
      stats[1, 12] <- skewness(x, na.rm = na.rm, type = type)
      stats[1, 13] <- kurtosis(x, na.rm = na.rm, type = type)
      
      vars <- 1
    }
    else {
      stats = matrix(rep(NA, ncol(x) * 13), ncol = 13)
      rownames(stats) <- colnames(x)
      stats[, 1] <- apply(x, 2, valid)
      vars <- c(1:ncol(x))
      for (i in seq_along(x)) {
        if (is.factor(x[[i]]) || is.logical(x[[i]])) {
          x[[i]] <- as.numeric(x[[i]])
          rownames(stats)[i] <- paste(rownames(stats)[i],
                                      "!#", sep = " ")
        }
        x[sapply(x, is.character)] <- lapply(x[sapply(x, is.character)], 
                                             as.factor)
        if (!is.numeric(unclass(x[[i]])))
          stop("'detail' still doesn't know how to deal with  'Date' or 'character' vectors")
      }
      stats[, 2] <- apply(x, 2, mean, na.rm = na.rm)
      if (!basic) {
        stats[, 12] <- sapply(x, FUN=skewness, na.rm = na.rm, type = type)
        stats[, 13] <- sapply(x, FUN=kurtosis, na.rm = na.rm, type = type)
      }
      stats[, 3] <- sapply(x, FUN=sd, na.rm = na.rm)
      stats[, 4] <- sapply(x, FUN=var, na.rm = na.rm)
      stats[, 5] <- sapply(x, FUN=se, na.rm = na.rm)
      stats[, 6] <- sapply(x, FUN=median, na.rm = na.rm)
      stats[, 7] <- sapply(x, FUN=mad, na.rm = na.rm)
      stats[, 8] <- sapply(x, FUN=mean, na.rm = na.rm, trim = trim)
      stats[, 9] <- sapply(x, FUN=winsorize, k=k, na.rm = na.rm)
      stats[, 10] <- sapply(x, FUN=min, na.rm = na.rm)
      stats[, 11] <- sapply(x, FUN=max, na.rm = na.rm)      
    }
    if (!basic) {
      if (!basic) {
        temp <- data.frame(obs = stats[, 1], 
                           mean = stats[, 2], 
                           sd = stats[, 3],  
                           var = stats[, 4], 
                           se = stats[, 5], 
                           median = stats[, 6], 
                           mad = stats[, 7],
                           trimmed = stats[, 8], 
                           winsor = stats[, 9],
                           range = stats[, 11] - stats[, 10],
                           min = stats[, 10], 
                           max = stats[, 11], 
                           skew = stats[, 12], 
                           kurt = stats[, 13])
      }
      else {
        temp <- data.frame(obs = stats[, 1], 
                           mean = stats[, 2], 
                           sd = stats[, 3],  
                           var = stats[, 4], 
                           se = stats[, 5], 
                           median = stats[, 6], 
                           mad = stats[, 7],
                           trimmed = stats[, 8],
                           winsor = stats[, 9],
                           range = stats[, 11] - stats[, 10],
                           min = stats[, 10], 
                           max = stats[, 11])
      }
    }
    else {
      if (!basic) {
        temp <- data.frame(obs = stats[, 1], 
                           mean = stats[, 2], 
                           sd = stats[, 3], 
                           var = stats[, 4], 
                           skew = stats[, 12], 
                           kurt = stats[, 13])
      }
      else {
        temp <- data.frame(obs = stats[, 1], 
                           mean = stats[, 2], 
                           sd = stats[, 3], 
                           var = stats[, 4], 
                           min = stats[, 10], 
                           max = stats[, 11] )
      }
    }
    output <- format(round(data.frame(vars = vars, temp), 1), nsmall = 0)
    
    class(output) <- c("SciencesPo", "summarize", "data.frame")
    return(output)   
  }
