#' @title Filling in missing values
#' \code{fillin} replaces missing values with existing observations from other variable or data frame.
#'
#' @param x the data frame with the variable you would like to fill in.
#' @param y the data frame with the variable you would like to use to fill in \code{x}.
#' @param x.var a character string of the name of the variable in \code{x} you want to fill in.
#' @param y.var an optional character string of variable name in \code{y} that you would like to use to fill in.
#' @param key a character vector of variable names that are shared by \code{x} and \code{y} that can be used to join the data frames.
#'
#' @examples
#' # Create data set with missing values
#' data1 <- data.frame(a = sample(c(1,2), 100, rep=TRUE),
#'                     b = sample(c(3,4), 100, rep=TRUE),
#'                    fNA = sample(c(100, 200, 300, 400, NA), 100, rep=TRUE))
#'
#' # Created full data set
#' data2 <- data.frame(a = c(1,2,1,2),
#'                      b = c(3,3,4,4),
#'                      full = c(100, 200, 300, 400))
#'
#' # Fill in missings from data1 with values from data2
#' Filled <- fillin(data1, data2, x.var = "fNA", y.var = "full", key = c("a", "b"))
#' @importFrom data.table data.table
#' @importFrom data.table  :=
#' @export
`fillin` <- function(x, y, x.var, y.var = NULL, key){
  # Give Var2 the same name as var1 if Var2 is NULL
  if (is.null(y.var)){
    y.var <- x.var
  } else {
    y.var <- y.var
  }
  # Give var a generic name
  names(x)[match(x.var, names(x))] <- "x_x"
  names(y)[match(y.var, names(y))] <- "x_y"
  # Convert data frames to data.table type objects
  tempx <- data.table(x, key = key)
  tempy <- data.table(y, key = key)
  # Merge data.tables
  outdata <- tempy[tempx]
  # Tell the user how many values will be filled in
  SubNA <- outdata[, list(x_x, x_y)]
  SubNA <- subset(SubNA, is.na(x_x) & !is.na(x_y))
  print(paste(nrow(SubNA), "NAs were replaced."))
  # Fill in missing values from data1 with values from data2
  outdata <- outdata[is.na(x_x), x_x := x_y]
  # Convert back to data frame
  ans <- data.frame(outdata)
  # Tell the user what the correlation coefficient is between the variables
  SubNoNA <- subset(ans, !is.na(x_x) & !is.na(x_y))
  HowMany <- nrow(SubNoNA)
  corr <- stats::cor(SubNoNA$x_x, SubNoNA$x_y, use = "complete.obs")
  print(paste("The correlation between", x.var, "and", y.var, "is", round(corr, digits = 3), "based on", HowMany, "shared observations." ))
  # Remove uncombined variable and return main variable's name
  names(ans)[match("x_x", names(ans))] <- x.var
  toKeep <- setdiff(names(ans), "x_y")
  ans <- ans[, toKeep]
  ans
}
NULL
