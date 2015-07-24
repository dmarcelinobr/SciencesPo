#' @encoding UTF-8
#' @title Join a list of data frames
#'
#' @description Recursively join data frames
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#'
#' @param x A list of data frames
#' @param \dots Arguments passed onto merge
#'
#' @examples
#' mtcars$cars <- row.names(mtcars)
#' df1 <- mtcars[, c(1:2, 12)]
#' df2 <- mtcars[, c(3:4, 12)]
#' df3 <- mtcars[, c(5:6, 12)]
#' joinLists(x = list(df1, df2, df3), by = "cars")
#' @export
`joinLists` <-
  function(x, ...)
  {
    dfs1 <- x[[1]]
    dfs2 <- x[-1]
    for(i in 1:length(dfs2)){
      dfs1 <- merge(dfs1, dfs2[[i]], all = TRUE, sort = FALSE, ...)
    }
    return(dfs1)
  }
NULL





#' @encoding UTF-8
#' @title Unnest a Nested List
#'
#' @description  Unnest nested lists made easy.
#'
#' @param x A nested list
#'
#' @return A list, with no nesting, hopefully
#'  @author Daniel Marcelino, \email{dmarcelino@@live.com}
#'
#' @examples
#' # Unnest the list
#' # a nested list
#' mylist <- list(); inerlist <- list()
#' for(i in 1:5) {
#'   for(j in 1:5) {
#'    mylist[[j]] <- i*j
#'  }
#'  inerlist[[i]] <- mylist
#' }
#' unnest(inerlist)[[1]]
#' unnest(inerlist)
#'
#' @export
`unnest` <-
  function(x) {
    if(is.null(names(x))) {
      list(unname(unlist(x)))
    }
    else {
      c(list(all=unname(unlist(x))), do.call(c, lapply(x, unnest)))
    }
  }
NULL






#' @encoding UTF-8
#' @title Reverse the levels of a factor.
#'
#' @param x a factor whose levels need to be reverse coded.
#'
#' @examples
#' mylevels <- c('Strongly Disagree', 'Disagree', 'Neither', 'Agree', 'Strongly Agree')
#'
#' test <- factor(sample(mylevels[1:5], 10, replace=TRUE))
#'
#' reverseLevels(test)
#'
#' cbind(test, as.integer(test), as.integer(reverseLevels(test)))
#'
#'
#'
#' @export
`reverseLevels` <- function(x) {
  if(is.factor(x)) {
    x <- factor(as.character(x), levels=rev(levels(x)), ordered=TRUE)
  } else if(is.data.frame(x)) {
    for(i in seq_along(x)) {
      if(is.factor(x[,i])) {
        x[,i] <- factor(as.character(x[,i]), levels=rev(levels(x[,i])), ordered=TRUE)
      } else {
        warning(paste0('Column ', i, ' is not a factor.'))
      }
    }
  } else {
    stop(paste0('Unsupported format: ', class(x)))
  }
  return(x)
}
NULL



#' @encoding UTF-8
#' @title Replace commas by dots
#'
#' @description Replace commas by dots in that order.
#'
#' @param x A vector whose elements contain commas or commas and dots.
#'
#' @details This function works for numeric vectors, typically currency variables stored in non-english format.
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#'
#' @keywords Data Manipulation
#'
#' @examples
#' x <- c('500,00', '0,001', '25.000', '10,100.10', 'him, you, and I.')
#'
#' commas2dots(x)
#'
#' @export
`commas2dots` <- function(x){
  round(as.numeric(gsub(",", ".", gsub("\\.", "", x))),2)
}
NULL




#' @encoding UTF-8
#' @title Rownames to column
#'
#' @description Moves rownames to column
#' @param data the data frame.
#' @param rowname the column name.
#'
#' @examples
#' x <- data.frame(c = c(87,8,8,87,38,92))
#' rownames(x) <- c("B", "H","I","J", "K","L")
#' rownames2col(x)
#' @export
`rownames2col` <-function(data,  rowname = "rowname") {
  ans <- data.frame(rownames(data), data, row.names = NULL)
  names(ans)[1] <- rowname
  ans
}
NULL



#' @title Converts rle object to data.frame
#'
#' @param r an rle object.
#'
#'
#' just converts an rle object to a data.frame
#   with columns: value, length, startIndex, endIndex
#' @export
`rle2data.frame` = function(r) {
  y <- data.frame(cbind(r[[2]], as.integer(r[[1]])),
                  stringsAsFactors=FALSE)
  y[,2] <- as.integer(y[,2])
  y <- cbind(y,cumsum(y[,2]))
  y <- cbind(y,(y[,3] - y[,2] + 1))
  y = y[,c(1,2,4,3)]
  names(y) = c("x","len","start","end")
  return(y)
}
NULL
