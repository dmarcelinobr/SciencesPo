#' @encoding UTF-8
#' @title Convert Factors into Numeric Vectors
#'
#' @description Convert Factors into Numeric Vectors
#'
#' @param x a factor whose levels will be converted.
#'
#' @examples
#' mylevels <- c('Strongly Disagree', 'Disagree', 'Neither', 'Agree', 'Strongly Agree')
#' myvar <- factor(sample(mylevels[1:5], 10, replace=TRUE))
#' unclass(myvar) # testing order
#' destring(myvar)
#'
#' @keywords Misc
#'
#' @export
`destring` <- function(x) {
  ## convert factor to strings
  if(is.character(x)) {
    as.numeric(x)
  } else if (is.factor(x)) {
    as.numeric(as.factor(x))
  } else if (is.numeric(x)) {
    invisible(x)
  } else {
    stop("Could not convert to numeric")
  }}
NULL




#' @encoding UTF-8
#' @title Generate dummy variables
#'
#' @description Provides an alternative to generate dummy variables
#'
#' @param x a column position to generate dummies
#' @param data the data object as a data.frame
#' @param drop A logical value. If \code{TRUE}, unused levels will be omitted
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#'
#' @details A matrix object
#'
#' @keywords Models
#'
#' @examples
#' df <- data.frame(y = rnorm(25), x = runif(25,0,1), sex = sample(1:2, 25, rep=TRUE))
#'
#' dummy(df$sex)
#'
#' @export
`dummy` <-
  function (x, data = NULL, drop = TRUE)
  {
    if (is.null(data)) {
      varname <- as.character(sys.call(1))[2]
      varname <- sub("^(.*\\$)", "", varname)
      varname <- sub("\\[.*\\]$", "", varname)
    }
    else {
      if (length(x) > 1)
        stop("More than one variable to create dummies at same  time.")
      varname <- x
      x <- data[, varname]
    }
    if (drop == FALSE && class(x) == "factor") {
      x <- factor(x, levels = levels(x), exclude = NULL)
    }
    else {
      x <- factor(x, exclude = NULL)
    }
    if (length(levels(x)) < 2) {
      warning(varname, " has only 1 dimension. Generating dummy variable anyway.")
      return(matrix(rep(1, length(x)), ncol = 1, dimnames = list(rownames(x),
                                                                 c(paste(varname, "_", x[[1]], sep = "")))))
    }
    mat <- model.matrix(~x - 1, model.frame(~x - 1), contrasts = FALSE)
    colnames.mm <- colnames(mat)
    cat(" ", varname, ":", ncol(mat), "dummy variables generated\n")
    mat <- matrix(as.integer(mat), nrow = nrow(mat), ncol = ncol(mat), dimnames = list(NULL,
                                                                                       colnames.mm))
    colnames(mat) <- sub("^x", paste(varname, "_", sep = ""), colnames(mat))
    if (!is.null(row.names(data)))
      rownames(mat) <- rownames(data)
    return(mat)
  }
NULL






#' @encoding UTF-8
#' @title Modify data elements by their position
#'
#' @description Modify an element in a vector, taking its position as reference.
#'
#' @param x A data object
#' @param position The position of the element to be replaced
#' @param value The value to modify
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#'
#' @keywords Data Manipulation
#
#' @examples
#'
#' x <- seq(1:10)
#'
#' modify(x, 1, 10)
#'
#' @export
#'
`modify` <-
  function(x, position, value) {
    x[position] <- value
    x
  }
NULL







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
#' @title Slice a vector
#'
#' @description Break up a vector by certain N sized chunks.
#'
#' @param x A numeric vector
#' @param by The number by which to split the vector
#' @param pattern The number of blocks
#' @details When using \code{pattern}, the formule used to break the vector is \code{length(x)/pattern)+1}.
#' @examples
#' x <- seq(1:15)
#' slice(x, by = 2)
#' slice(x, pattern = 4)
#' slice(sample(x), by= 2) # draw random pairs
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#'
#' @export
`slice` <-
  function(x, by = 2, pattern  = NULL) {
    if(is.null(pattern)){
      starts <- seq(1, length(x), by)
      tt <- lapply(starts, function(y) x[y:(y + (by - 1))])
      lapply(tt, function(x) x[!is.na(x)])
    } else {
      splitby <- round(length(x)/pattern)+1
      starts <- seq(1, length(x), splitby)
      tt <- lapply(starts, function(y) x[y:(y + (splitby - 1))])
      lapply(tt, function(x) x[!is.na(x)])
    }
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
#' commas4dots(x)
#'
#' @export
`commas4dots` <- function(x){
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





#' @encoding UTF-8
#' @title Compute z-scores
#'
#' @description Compute z-scores
#' @param x a numeric vector
#' @param na.rm a logical indicating whether missing values should be removed
#' @export
#' @examples
#' x <- sample(10)
#' zscores(x)
`zscores` <- function( x, na.rm=getOption("na.rm", FALSE) ) {
  ( x - mean(x, na.rm=na.rm)) / sd(x, na.rm=na.rm)
}
NULL





#' @encoding UTF-8
#' @title Unity-based normalization
#'
#' @description Normalizes as feature scaling, \code{min - max}, or unity-based normalization. Typically used to bring all values into the range [0,1]. However, this can be generalized to restrict the range of values in the dataset between any arbitrary points  \code{a}  and  \code{b}, using: \deqn{X' = a + \frac{(x - x_{min})(b - a)}{(x_{max} - x_{min})}}.
#'
#' @param x is a vector to be normalized.
#' @param range is a numeric vector of length 2 for min and max values, default is \code{c(0,1)}.
#' @param domain a numeric vector of length 2.
#' @param \dots further arguments passed to or used by other methods.
#' @return Normalized values in an object of the same class as \code{var}.
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @seealso svTransform
#'
#' @examples
#' x <- sample(10)
#' normalize(x, range=c(0,1))
#' normalize(x)
#'
#' @keywords Rescaling
#' @keywords Normalization
#' @seealso  \code{\link{scale}}.
#'
#' @export
`normalize` <- function(x, range, domain, ...) {
  UseMethod("normalize")
}

#' @rdname normalize
#' @export
`normalize.factor` <- function(x, range, domain=range(1:nlevels(x)), ...) {
  width <- diff(range)
  n <- length(levels(x)) - 1
  range[1]  - 1/n + width * as.numeric(x) / n
}

#' @rdname normalize
#' @export
`normalize.numeric` <- function(x, range=c(0,1), domain=range(x, na.rm=TRUE), ...) {
  range_width  <- diff(range)
  domain_width <- diff(domain)
  range[1] + range_width * (x - min(x)) / domain_width
}

#' @rdname normalize
#' @export
`normalize.default` <- function(x, range=c(0,1), domain, ...) {
  normalize( as.numeric(x, range=range, domain, ...) )
}

#' @rdname normalize
#' @export
`normalize.character` <- function(x, range=c(0,1), domain, ...) {
  normalize( as.factor(x), range=range, domain=domain)
}
NULL



#' @encoding UTF-8
#' @title Transform dependent variable
#' @description Simple function to transform a dependent variable that in [0,1] rather than (0, 1) to beta regression. Suggested by Smithson & Verkuilen (2006).
#'
#' @param y the dependent variable in [0, 1] interval.
#' @references
#' Smithson M, Verkuilen J (2006) A Better Lemon Squeezer? Maximum-Likelihood Regres- sion with Beta-Distributed Dependent Variables. \emph{Psychological Methods}, 11(1), 54-71.
#'
#' @seealso normalize
#' @examples
#'  x <- sample(10);
#'  y <- normalize(x, range=c(0,1));
#'  y;
#'  svTransform(y)
#' @export
`svTransform` <- function(y)
{
  n <- length(y)
  trans <- (y * (n-1) + 0.5)/n
  return(trans)
}
NULL




#' @encoding UTF-8
#' @title Splits name fields
#' @description Splits a name field variable allocating the first and last names into two new columns or a list.
#' @param name the name field column.
#' @param data the data.frame name.
#'
#' @return two columns or a list.
#' @seealso \link{unnest}.
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @details The way one may split names is region dependent, so this function may apply to very few contexts. See for instance \url{http://www.w3.org/International/questions/qa-personal-names}
#'
#' @examples
#'  df <- data.frame( name = c("Martin Luther King",
#'  "Nelson Mandela", "Simon Bolivar") )
#'  name.split(df$name)
#'  df$n<- name.split(df$name)
#' @export
`name.split`<- function(name, data=.data){
  .data <- NULL
  #nl <- as.list(1:ncol(data))
  # names(nl) <- names(data)
  # - TODO maybe warn about replacing existing variable with the same names (first and last)
  first = as.character(
    lapply(
      strsplit(
        as.character(
          name), split='\\s+'),
      head, n=1))

  last = as.character(
    lapply(
      strsplit(
        as.character(
          name), split='\\s+'),
      tail, n=1))
  if(!missing(data)){
    return(cbind(data, first, last))
  }else{
    return(cbind(first, last))
  }
}
NULL




#' @encoding UTF-8
#' @title Extraction of categorical values as a preprocessing step for making dummy variables
#'
#' @description  \code{categories} stores all the categorical values that are present in the factors and character vectors of a data frame. Numeric and integer vectors are ignored. It is a preprocessing step for the \code{dummy} function. This function is appropriate for settings in which the user only wants to compute dummies for the categorical values that were present in another data set. This is especially useful in predictive modeling, when the new (test) data has more or other categories than the training data.
#'
#' @param x data frame containing factors or character vectors that need to be transformed to dummies. Numerics, dates and integers will be ignored.
#' @param p select the top p values in terms of frequency. Either "all" (all categories in all variables), an integer scalar (top p categories in all variables), or a vector of integers (number of top categories per variable in order of appearance.
#' @examples
#' #create toy data
#' (traindata <- data.frame(xvar=as.factor(c("a","b","b","c")),
#'                          yvar=as.factor(c(1,1,2,3)),
#'                          var3=c("val1","val2","val3","val3"),
#'                          stringsAsFactors=FALSE))
#' (newdata <- data.frame(xvar=as.factor(c("a","b","b","c","d","d")),
#'                        yvar=as.factor(c(1,1,2,3,4,5)),
#'                        var3=c("val1","val2","val3","val3","val4","val4"),
#'                        stringsAsFactors=FALSE))
#'
#' categories(x=traindata,p="all")
#' categories(x=traindata,p=2)
#' categories(x=traindata,p=c(2,1,3))
#' @seealso \code{\link{dummy}}
#' @return  A list containing the variable names and the categories
#' @author Authors: Michel Ballings, and Dirk Van den Poel, Maintainer: \email{Michel.Ballings@@GMail.com}
#' @export
`categories` <- function(x, p="all"){
  categoricals <- which(sapply(x,function(x) is.factor(x) || is.character(x)))
  x <- data.frame(x[,categoricals])
  cats <- sapply(1:ncol(x),function(z) {
    cats <- table(x[,z])
    if(is.numeric(p) && length(p) == 1) {
      names(sort(cats,decreasing=TRUE)[1:if(length(cats) <= p) length(cats) else p])
    } else if (is.numeric(p) && length(p) >= 1) {
      names(sort(cats,decreasing=TRUE)[1:if(length(cats) <= p[z]) length(cats) else p[z]])
    } else if (p=="all") {
      names(cats)
    }
  },simplify=FALSE)
  names(cats) <- names(x)
  cats
}
NULL



