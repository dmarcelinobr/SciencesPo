#' @encoding UTF-8
#' @title Subset data 
#' 
#'  @description Subsets a \code{data.frame} based on variables or/and records. It is a version of \sQuote{subset.data.frame} which is a standard R function.
#'  
#'  @param data = .data
#'  #'  @param select the columns to select from \code{data}.
#'  @param exclude the columns to remove from \code{data}.
#'  @param subset the elements or rows to keep from \code{data} (missing values are taken as false).
#'  @param drop passed on to [ indexing operator.
#'  @param refactor whether the levels of variable(s) with zero count should be removed after subsetting. The default is \code{refactor="subset.vars"}, which means that the levels of the variables not being used will be recycled.
#' @param sample an integer for the size of random sample to retain from the \code{data}.
#' @param \dots typically unecessary parameters.
#'  
#' @examples
#' data(ssex)
#' keep(ssex, select = c(Date, Oppose, Favor))

#' @export
#'
keep <-
  function (data = .data, select, exclude = NULL, subset, drop = FALSE, refactor = c("subset.vars", "all", "none"), sample = NULL, ...) 
  {

    data.name <- as.character(substitute(data))
    data1 <- data
    datalabel <- attr(data1, "datalabel")
    val.labels <- attr(data1, "val.labels")
    var.labels <- attr(data1, "var.labels")
    label.table <- attr(data1, "label.table")
    if (!is.null(sample)) {
      if (!is.numeric(sample) | sample <= 0 | length(sample) > 
            1 | (trunc(sample) != sample) & sample > 1) {
        stop("Size of sample must be a positive integer")
      }
      if (sample < 1) {
        sample0 <- sample
        sample <- trunc(sample * nrow(data1))
        cat("Keep only ", round(sample0 * 100, 2), "% or ", 
            sample, " of the total ", nrow(data1), " records", 
            "\n", sep = "")
      }
      data <- data[sample(nrow(data), sample), 
                             ]
      data1 <- data
      attr(data1, "datalabel") <- paste(datalabel, "(subset)")
      attr(data1, "val.labels") <- val.labels
      attr(data1, "var.labels") <- var.labels
      attr(data1, "label.table") <- label.table
    }
    if (missing(subset)) 
      r <- TRUE
    else {
      e <- substitute(subset)
      r <- eval(e, data, parent.frame())
      if (!is.logical(r)) 
        stop("'subset' must evaluate to logical")
      r <- r & !is.na(r)
    }
    if (missing(select)) {
      vars <- TRUE
      if (suppressWarnings(!is.null(exclude))) {
        nl <- as.list(1:ncol(data))
        names(nl) <- names(data)
        if ((length(grep(pattern = "[*]", as.character(substitute(exclude)))) == 
               1) | (length(grep(pattern = "[?]", as.character(substitute(exclude)))) == 
                       1)) {
          vars <- -grep(pattern = glob2rx(as.character(substitute(exclude))), 
                        names(data))
          if (length(vars) == 0) {
            stop(paste(as.character(substitute(exclude)), 
                       "not matchable with any variable name."))
          }
        }
        else {
          vars <- -eval(substitute(exclude), nl, parent.frame())
        }
      }
    }
    else {
      nl <- as.list(1:ncol(data))
      names(nl) <- names(data)
      if ((length(grep(pattern = "[*]", as.character(substitute(select)))) == 
             1) | (length(grep(pattern = "[?]", as.character(substitute(select)))) == 
                     1)) {
        vars <- grep(pattern = glob2rx(as.character(substitute(select))), 
                     names(data))
        if (length(vars) == 0) {
          stop(paste(select, "not matchable with any variable name."))
        }
      }
      else {
        vars <- eval(substitute(select), nl, parent.frame())
      }
    }
    data1 <- data[r, vars, drop = drop]
    attr(data1, "datalabel") <- paste(datalabel, "(subset)")
    attr(data1, "val.labels") <- val.labels[vars]
    attr(data1, "var.labels") <- var.labels[vars]
    attr(data1, "label.table") <- label.table[is.element(names(label.table), 
                                                         val.labels[vars])]
    if(length(refactor)==3) refactor <- "subset.vars"
    if(!missing(subset) & refactor == "all") {
      for(i in 1:ncol(data1)) {
        if(class(data1[,i]) == "factor") {
          data1[,i] <- factor(data1[,i])
        }
      }
    }
    if(!missing(subset) & refactor == "subset.vars") {
      for(i in 1:ncol(data1)) {
        if(length(grep(names(data1)[i], deparse(substitute(subset)))) >0 
           & class(data1[,i]) == "factor") {
          data1[,i] <- factor(data1[,i])
        }
      }
    }
    assign(data.name, data1, pos = 1)
    if (is.element(data.name, search())) {
      detach(pos = which(search() %in% data.name))
      attach(data1, name = data.name, warn.conflicts = FALSE)
    }
  }
NULL






#' @encoding UTF-8
#' @title Recode variable
#' 
#' @description Change values of a variable in a \code{data.frame}.
#' label.var
#' @param vars varaible(s) to be recoded 
#' @param from old values or arithmetic conditions. 
#' @param to new values for all variables listed.
#' @param data a \code{data.frame} object.
#' @param \dots typically not needed parameters. 
#' 
#' @examples
#' df = data.frame(id=1:20, x=rnorm(20, mean=2, sd=.5), 
#' z=sample(5, 20, rep=TRUE) )
#' use(df)
#' # recoding x  to missing value:
#' recode(z, from = c(1,2,3,4,5), to = c(5,4,3,2,1), data=df)
#' @export
recode <-
  function (vars, from, to, data = .data, ...) 
  {
    data1 <- data
    nl <- as.list(1:ncol(data1))
    names(nl) <- names(data1)
    var.order <- eval(substitute(vars), nl, parent.frame())
    if(all(var.order < 0)) var.order <- (1:ncol(data))[var.order]
    if (exists(names(data1)[var.order], where = 1, inherits = FALSE)) 
      warning("Name(s) of vars duplicates with an object outside the `data`.")
    tx <- cbind(from, to)
    if (is.numeric(from) | is.integer(from) | any(class(data1[, 
                                                                        var.order]) == "POSIXt")) {
      if (length(from) == 1) {
        if(all(is.integer(data1[, var.order]))){
          data1[, var.order][data1[, var.order] == from] <- as.integer(to)
        }else{
          data1[, var.order][data1[, var.order] == from] <- to
        }
        
      }
      else {
        if (length(from) != length(to) & length(to) != 
              1) 
          stop("Lengths of old and new values are not equal")
        for (i in var.order) {
          if(is.integer(data1[,i])){
            data1[, i] <- as.integer(lookup(data1[, i, drop = TRUE], 
                                            tx))
            
          }else{
            data1[, i] <- lookup(data1[, i, drop = TRUE], 
                                 tx)
          }
        }
      }
    }
    else for (i in var.order) {
      if (is.factor(data1[, i])) {
        if (length(from) != length(to) & length(to) != 
              1) 
          stop("Lengths of `from` and `to` are not equal")
        if (is.character(from)) {
          if (any(!is.element(from, levels(data1[, 
                                                      i])))) 
            warning(paste("The from is/are not element of levels of '", 
                          names(data1)[i], "'", sep = ""))
          for (j in 1:nrow(tx)) {
            levels(data1[, i])[levels(data1[, i]) == tx[j, 
                                                        1]] <- tx[j, 2]
          }
        }
      }
      if (is.character(data1[, i])) {
        if (length(from) == 1) {
          data1[, var.order][data1[, var.order] == from] <- to
        }
        else {
          if (length(from) != length(to) & 
                length(to) != 1) 
            stop("Lengths of old and new values are not equal")
          data1[, i] <- lookup(data1[, i, drop = TRUE], 
                               tx)
        }
      }
    }
    if (length(from) == nrow(data1)) {
      if (length(var.order) == 1) {
        data1[, var.order] <- replace(data1[, var.order], 
                                      from, to)
      }
      else {
        for (i in 1:length(var.order)) {
          data1[, var.order[i]] <- replace(data1[, var.order[i]], 
                                           from, to)
        }
      }
    }
    assign(as.character(substitute(data)), data1, pos = 1)
    if (is.element(as.character(substitute(data)), search())) {
      detach(pos = which(search() %in% as.character(substitute(data))))
      attach(data1, name = as.character(substitute(data)), 
             warn.conflicts = FALSE)
    }
  }
NULL





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
destring <- function(x) {
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
dummy <-
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
modify <-
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
#' df1 <- mtcars[,c(1:2,12)]
#' df2 <- mtcars[,c(3:4,12)]
#' df3 <- mtcars[,c(5:6,12)]
#' joinLists(x=list(df1, df2, df3), by="cars")

#' @export
joinLists <-
  function(x, ...)
  {
    dfs1 <- x[[1]]
    dfs2 <- x[-1]
    for(i in 1:length(dfs2)){
      dfs1 <- merge(dfs1, dfs2[[i]], all = TRUE, sort = FALSE, ...)
    }
    return( dfs1 )
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
unnest <-
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
reverseLevels <- function(x) {
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
#' @title Lag or Lead Observations
#' 
#' @description Shift function allows one to either lag or lead a column variables in a data frame.
#' 
#' @param x the variable to be lagged or leaded
#' @param id the subject or identification variable.
#' @param time the time id variable. 
#' @param delta an integer value (positive or negative) for units to move either backward or forward.
#' 
#' @details The combination of \code{id} and \code{time} must yelds to a unique identification of the observations.
#' 
#' @return An object of the same type as \code{x}
#' 
#' @examples 
#' data(sheston91)
#' attach(sheston91)
#' peek(sheston91)
#' ## lag
#' sheston91$L.pop <- shift(x = pop, id = country, time = year, delta = 1) 
#' head(sheston91)
#' 
#' # lead
#'  sheston91$pop.L <- shift(x = pop, id = country, time = year, delta =  -1) 
#' head(sheston91)
#' 
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' 
#' @export
#'
shift <- function (x, id, time, delta = 1) 
{
  if (!is.integer(delta)) 
    delta <- as.integer(delta)
  if (length(id) != length(time)) 
    stop("The length of these two variables must be equal")
  if (any(duplicated(paste(id, time)))) 
    stop("The combination of id and time must be unique")
  if (any(data.frame(id, time) != data.frame(id[order(id,  time)], time[order(id, time)]))) {
    new.order <- order(id, time)
    x <- x[new.order]
    id <- id[new.order]
    time <- time[new.order]
  }
  x.shift <- x
  id.shift <- id
  time.shift <- time
  if (delta >= 1) {
    x.shift[length(id):(delta + 1)] <- x[(length(id) - delta):1]
    x.shift[1:delta] <- NA
    id.shift[length(id):(delta + 1)] <- id[(length(id) - 
                                              delta):1]
    time.shift[length(id):(delta + 1)] <- time[(length(id) -  delta):1]
  }
  else {
    x.shift[1:(length(id) + delta)] <- x[(-delta + 
                                            1):length(id)]
    x.shift[length(id):(length(id) + delta + 1)] <- NA
    id.shift[1:(length(id) + delta)] <- id[(-delta + 
                                              1):length(id)]
    time.shift[1:(length(id) + delta)] <- time[(-delta + 
                                                  1):length(id)]
  }
  x.shift[id != id.shift] <- NA
  if(exists("new.order")){
    x.shift <- x.shift[order(new.order)]
  }
  return(x.shift)
}
NULL
#' 
# shift <-
#function(x, delta=NA){
#  stopifnot(is.numeric(delta))
#  stopifnot(is.numeric(x))
#  
#  if (length(delta)>1)
#    return(sapply(delta,shift, x=x))
#  
#  output<-NULL
#  abs.delta=abs(delta)
#  if (delta > 0 )
#    output<-c(tail(x,-abs.delta),rep(NA,abs.delta))
#  else if (delta < 0 )
#    output<-c(rep(NA,abs.delta), head(x,-abs.delta))
#  else 
#    output <- x
#  return(output)
#}





#' @encoding UTF-8
#' @title Slice a vector
#' 
#' @description Break up a vector by certain N sized chunks.
#' 
#' @param x A numeric vector
#' @param by The number by which to split the vector
#' @param pattern The number of blocks
#' 
#' @examples
#' x <- seq(1:15)
#' slice(x, by = 2, pattern = 4)
#'  
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' 
#' @export
slice <-
  function(x, by = 2, pattern  = NULL) {
    if(is.null(pattern)){	
      starts <- seq(1, length(x), by)
      tt <- lapply(starts, function(y) x[y:(y + (by - 1))])
      lapply(tt, function(x) x[!is.na(x)])
    } else
    {
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
#' replaceCommas(x)
#'
#' @export
replaceCommas <- function(x){
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
rownames2col <-function(data,  rowname = "rowname") {
  ans <- data.frame(rownames(data), data, row.names = NULL)
  names(ans)[1] <- rowname
  ans
}
NULL




# For 'recode'ing missing values of one or more variables into a new value
# NAto0 <-
#  function (vars, value=0, data = .data, ...){
#    data1 <- data
#    nl <- as.list(1:ncol(data1))
#    names(nl) <- names(data1)
#    var.order <- eval(substitute(vars), nl, parent.frame())
#    if (exists(names(data1)[var.order], where = 1, inherits = FALSE))
#      warning("Name(s) of vars duplicates with an object outside the data.")
#    for (i in var.order) {
#      temp.vector <- data1[, i, drop=TRUE]
#      if (is.factor(temp.vector)){
#        levels(temp.vector) <- c(levels(temp.vector), value)
#      }
#      temp.vector[is.na(temp.vector)] <- value
#      temp.vector -> data1[, i]
#    }
#    assign(as.character(substitute(data)), data1, pos = 1)
#    if (is.element(as.character(substitute(data)), search())) {
#      detach(pos = which(search() %in% as.character(substitute(data))))
#      attach(data1, name = as.character(substitute(data)),
#             warn.conflicts = FALSE)
#    }
# }
#






#' @encoding UTF-8
#' @title Lookup
#' 
#'  @description Recodes values of a vector from a lookup array. 
#'  
#'  @param x the variable
#'  @param  lookup.array a n-by-2 array used for looking up.
#'
#'  
#' @export
lookup <- function (x, lookup.array) 
{
  if (any(table(lookup.array[, 1]) > 1)) {
    stop("Index value in lookup array not unique!!")
  }
  else{
    b <- rep("", length(x))
    for (i in 1:nrow(lookup.array)) {
      if(is.na(lookup.array[i,1]) & !is.na(lookup.array[i,2])){
        b[is.na(x)] <- lookup.array[i,2]
      }else{
        b[x == lookup.array[i, 1]] <- as.character(lookup.array[i, 2])
      }
    }
    if(is.numeric(lookup.array)){
      x[b != "" & !is.na(b)] <- as.numeric(b[b != "" & !is.na(b)])
    }else{
      x[b != "" & !is.na(b)] <- (b[b != "" & !is.na(b)])
    }
    x[is.na(b)] <- as.numeric(b[is.na(b)])
    answer <- x
    return(answer)
  }
}
NULL





#' @encoding UTF-8
#' @title Wrap all related variables 
#' 
#' @description  Try to  wrap all related variables into the existing .data
#' @param data is the .data object
#'
#' @export
wrap <- function (data = .data) 
{
  data1 <- data
  j <- NULL
  k <- attr(data1, "var.labels")
  candidate.objects <- setdiff(lsNoFunction(), as.character(ls.str(mode = "list")[]))
  if (length(candidate.objects) == 0) 
    stop("No related vector outside the default data frame")
  for (i in 1:length(candidate.objects)) {
    if (length(get(candidate.objects[i])) == nrow(data1)) {
      if (any(names(data1) == candidate.objects[i])) {
        data1[, names(data1) == candidate.objects[i]] <- get(candidate.objects[i])
        j <- c(j, i)
      }
      else {
        data1 <- data.frame(data1, get(candidate.objects[i]))
        names(data1)[ncol(data1)] <- candidate.objects[i]
        j <- c(j, i)
        if (!is.null(k)) {
          k <- c(k, "")
        }
      }
    }
  }
  attr(data1, "var.labels") <- k
  rm(list = candidate.objects[j], pos = 1)
  assign(as.character(substitute(data)), data1, pos=1)
  if(is.element(as.character(substitute(data)), search())){
    detach(pos=which(search() %in% as.character(substitute(data))))
    attach(data1, name=as.character(substitute(data)), warn.conflicts = FALSE)
  }
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
rleFrame = function(r) {
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
#' zscore(x)
zscore <- function( x, na.rm=getOption("na.rm", FALSE) ) {
  ( x - mean(x, na.rm=na.rm)) / sd(x, na.rm=na.rm)
}
NULL




#' @encoding UTF-8
#' @title Unity-based normalization
#' 
#' @description Normalizes as feature scaling, \code{min - max}, or unity-based normalization. Typically used to bring all values into the range [0,1]. However, this can be generalized to restrict the range of values in the dataset between any arbitrary points  \code{a}  and  \code{b}, using: \deqn{X' = a + \frac{(x - x_{min})(b - a)}{(x_{max} - x_{min})} }.
#' 
#' @param x is a vector to be normalized.
#' @param range isa numeric vector of length 2 for min and max values, default is \code{c(0,1)}.
#' @param domain a numeric vector of length 2.
#' @param ... additional arguments.
#' @return Normalized values in an object of the same class as \code{var}.
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#'
#'
#' @examples
#' x <- sample(10)
#' normalize(x, range=c(0,1))
#' normalize(x)
#' 
#' @keywords Rescaling
#'
#' @seealso  \code{\link{scale}}, \code{\link{unscale}}
#' @export
normalize <- function(x, range, domain, ...) {
  UseMethod("normalize")
}

#' @rdname normalize
#' @export
normalize.factor <- function(x, range, domain=range(1:nlevels(x)), ...) {
  width <- diff(range)
  n <- length(levels(x)) - 1
  range[1]  - 1/n + width * as.numeric(x) / n
}

#' @rdname normalize
#' @export
normalize.numeric <- function(x, range=c(0,1), domain=range(x, na.rm=TRUE), ...) {
  range_width  <- diff(range)
  domain_width <- diff(domain)
  range[1] + range_width * (x - min(x)) / domain_width
}

#' @rdname normalize
#' @export
normalize.default <- function(x, range=c(0,1), domain, ...) {
  normalize( as.numeric(x, range=range, domain, ...) )
}

#' @rdname normalize
#' @export
normalize.character <- function(x, range=c(0,1), domain, ...) {
  normalize( as.factor(x), range=range, domain=domain)
}
NULL




#' @encoding UTF-8
#' @title Splits name field variable 
#' @description Splits a name field variable allocating the first and last names into two new columns or a list.
#' @param name the name field column. 
#' @param data the data.frame name.
#' 
#' @return two columns or a list.
#' @seealso \link{unnest}.
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @details The way one may split a name is region dependent, so this function may only apply to very few contexts. See for instance \url{http://www.w3.org/International/questions/qa-personal-names} 
#' @examples
#' df <- data.frame( name = c("Martin Luther King", "Nelson Mandela", "Simon Bolivar") )
#' nameSplit(df$name)
#' df$n<- nameSplit(df$name)
#'  # df[]<- nameSplit(df$name)
#' @export
nameSplit<- function(name, data=.data){
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
#' @title Extraction of Categorical Values as a Preprocessing Step for Making Dummy Variables
#' 
#' @description  \code{categories} stores all the categorical values that are present in the factors and character vectors of a data frame. Numeric and integer vectors are ignored. It is a preprocessing step for the \code{dummy} function. This function is appropriate for settings in which the user only wants to compute dummies for the categorical values that were present in another data set. This is especially useful in predictive modeling, when the new (test) data has more or other categories than the training data.
#'
#' @param x data frame containing factors or character vectors that need to be transformed to dummies. Numerics, dates and integers will be ignored.
#' @param p select the top p values in terms of frequency. Either "all" (all categories in all variables), an integer scalar (top p categories in all variables), or a vector of integers (number of top categories per variable in order of appearance.
#' @examples
#' #create toy data
#' (traindata <- data.frame(var1=as.factor(c("a","b","b","c")),
#'                          var2=as.factor(c(1,1,2,3)),
#'                          var3=c("val1","val2","val3","val3"),
#'                          stringsAsFactors=FALSE))
#' (newdata <- data.frame(var1=as.factor(c("a","b","b","c","d","d")),
#'                        var2=as.factor(c(1,1,2,3,4,5)),
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
categories <- function(x,p="all"){
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

