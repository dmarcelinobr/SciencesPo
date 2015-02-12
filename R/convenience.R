#' @encoding UTF-8
#' @title Attach exclusively various file formats
#' 
#' @description This works rigorously as the \pkg{epicalc}'s \code{use} function, though limited for the file formats it can read. Fundamentally, it replaces the command attach of R and save an object with extension \code{.data}, which becomes the default dataset. All other \code{data.frames} will be detached, by the time of using the \code{use} function, unless the argument \code{clear = FALSE} is specified.
#' 
#' @param file the name of the file which the data are to be read from.
#' @param data the internal name after attaching the data file.
#' @param clear if \code{clear = TRUE}, all attached data in the environment will be detached first.
#' @param spss.missing whether SPSS missing values should be replaced with NA; default is \code{spss.missing = TRUE}.
#' @param tolower  whether variable names should be forced to lower case; default is \code{tolower = TRUE}.
#' 
#' @details By using this \dQuote{attach} version, the data becomes available globally usually positioned in the second place, \code{search()}. 
#' 
#' @importFrom foreign read.dta 
#' @importFrom foreign read.spss
#' 
#' @examples
#' data(ssex)
#' use(ssex)
#' 
#' @export

use <-
  function (file,  data = .data, clear = TRUE, spss.missing = TRUE, tolower = TRUE) 
  { 
    if (clear) {
      detachAll()
    }
    .data <- NULL
    
    if (is.character(file)) {
      ext <- tolower(substring(file, first = nchar(file) - 
                                 3, last = nchar(file)))
      if (ext == ".dta") {
        dataset <- read.dta(file)
      }
      else {
        if (ext == ".sav") {
          data0 <- read.spss(file)
          var.labels <- attr(data0, "variable.labels")
          dataset <- read.spss(file, to.data.frame=TRUE, trim.factor.names=TRUE)
          dataset <- dataset[1:nrow(dataset), 1:ncol(dataset)]
          attr(dataset, "var.labels") <- var.labels
          if(spss.missing){
            for(i in 1:ncol(dataset)){
              if(!is.null(attr(data0, "missing")[[i]]$value)){
                dataset[,i] <- ifelse((dataset[,i] %in% attr(data0, "missing")[[i]]$value),NA,dataset[,i])
              }
              if(!is.null(attributes(data0[[i]])$value.labels)){
                dataset[,i] <- ifelse((dataset[,i] %in% attributes(data0[[i]])$value.labels),NA,dataset[,i])
              }
            }
            if (tolower) 
              names(dataset) <- tolower(names(dataset))
          }
          else {
            if (substring(file, first = nchar(file) - 
                            3, last = nchar(file)) == ".tsv") {
              dataset <- read.delim(file, header = TRUE, 
                                    sep = "\t", stringsAsFactors=FALSE)
            }
            else {
              if (substring(file, first = nchar(file) - 
                              3, last = nchar(file)) == ".csv") {
                dataset <- read.csv(file, header = TRUE, 
                                    sep = ",", stringsAsFactors=FALSE )
              }
              else {
                stop("This type of file cannot be 'used'.")
              }
            }
          }
        }
      }
    }
    else {
      if (is.data.frame(file)) {
        dataset <- file
      }
      else {
        stop("The argument is not a data frame or no such file")
      }
    }
    pos = 1 # does nothing just to trick the environment 
    assign(as.character(substitute(data)), dataset, envir = as.environment(pos))
    message(paste0("file assigned to `.data`"))
    attach(dataset, name = as.character(substitute(data)), 
           warn.conflicts = FALSE)
  }
NULL





#' @encoding UTF-8
#' @title Get Information on Data Objects 
#'
#' @param data the data frame to be detailed.
#' @param show the selection of columns from \code{data}, if not all.
#' @param ignore columns from \code{data} to prevent of showing.
#' 
#' @examples
#' data(titanic)
#' ## Wildcard for variables
#' info(titanic, "C*") # Show all variables starting with 'C'
#' ## Subset of variables
#' info(titanic, show = CLASS:SEX) # Same results
#' info(titanic, show = 1:3)
#' ## Exclusion using wildcard.
#' info(titanic, ignore = "C*")
#' 
#' @export
info <- function (data, show, ignore) 
{
  if (!missing(show) | !missing(ignore)) {
    nl <- as.list(1:ncol(data))
    names(nl) <- names(data)
    if (!missing(show)) 
      vars.shown <- eval(substitute(show), nl, parent.frame())
    if (!missing(ignore)) 
      vars.ignored <- eval(substitute(ignore), nl, parent.frame())
    if ((length(grep(pattern = "[*]", as.character(substitute(show)))) == 
           1) | (length(grep(pattern = "[?]", as.character(substitute(show)))) == 
                   1)) {
      vars.shown <- grep(pattern = glob2rx(as.character(substitute(show))), 
                         names(data))
      if (length(vars.shown) == 0) {
        stop(paste(show, "not matchable with any variable name."))
      }
    }
    if ((length(grep(pattern = "[*]", as.character(substitute(ignore)))) == 
           1) | (length(grep(pattern = "[?]", as.character(substitute(ignore)))) == 
                   1)) {
      vars.ignored <- grep(pattern = glob2rx(as.character(substitute(ignore))), 
                           names(data))
      if (length(vars.ignored) == 0) {
        stop(paste(ignore, "not matchable with any variable name."))
      }
    }
    vars <- 1:ncol(data)
    if (exists("vars.shown")) 
      vars <- vars[vars.shown]
    if (exists("vars.ignored")) 
      vars <- vars[-vars.ignored]
    dataset <- data[1,]
    class.a <- rep("", length(vars))
    for (i in 1:length(vars)) {
      class.a[i] <- class(dataset[,vars[i]])[1]
    }
    if (is.null(attr(data, "var.labels"))) {
      a <- cbind(colnames(dataset)[vars], class.a, rep("", 
                                                       length(vars)))
    }
    else {
      a <- cbind(colnames(dataset)[vars], class.a, attr(data, 
                                                        "var.labels")[vars])
    }
    colnames(a) <- c("Variable     ", "Class          ", 
                     "Description")
    rownames(a) <- vars
    header <- paste(attr(data, "datalabel"), "\n",.No.of.observations,nrow(data), "\n")
    options(warn = 0)
  }
  else {
    if (!is.data.frame(data)) {
      if (is.character(data) & (length(grep(pattern = "[*]", 
                                            data)) == 1) | (length(grep(pattern = "[?]", data) == 
                                                                     1))) {
        vars <- grep(pattern = glob2rx(data), names(data))
        if (length(vars) == 0) {
          stop(paste(data, "not matchable with any variable name."))
        }
        
        dataset <- data[1,]
        class.a <- rep("", length(vars))
        for (i in 1:length(vars)) {
          class.a[i] <- class(dataset[,vars[i]])[1]
        }
        if (is.null(attr(data, "var.labels"))) {
          a <- cbind(colnames(dataset)[vars], class.a, 
                     rep("", length(vars)))
        }
        else {
          a <- cbind(colnames(dataset)[vars], class.a, 
                     attr(data, "var.labels")[vars])
        }
        colnames(a) <- c("Variable     ", "Class          ", 
                         "Description")
        rownames(a) <- vars
        header <- paste(attr(data, "datalabel"), "\n",.No.of.observations,nrow(data), "\n")
        options(warn = 0)
      }
      else {
        candidate.position <- NULL
        for (search.position in 1:length(search())) {
          if (exists(as.character(substitute(data)), where = search.position)) {
            if (any(names(get(search()[search.position])) == 
                      as.character(substitute(data))) | any(ls(all.names = TRUE, 
                                                               pos = 1) == as.character(substitute(data)))) 
              candidate.position <- c(candidate.position, 
                                      search.position)
          }
        }
        var.order <- as.character(NULL)
        var.class <- NULL
        var.size <- NULL
        var.lab <- NULL
        for (i in candidate.position) {
          if (i == 1) {
            var.order <- c(var.order, "")
          }
          else {
            var.order <- c(var.order, which(as.character(substitute(data)) == 
                                              names(get(search()[i]))))
          }
          if (i == 1) {
            var.class <- c(var.class, class(data))
          }
          else {
            var.class <- c(var.class, class(get(search()[i])[, 
                                                             which(as.character(substitute(data)) == names(get(search()[i])))]))
          }
          if (i == 1) {
            var.size <- c(var.size, length(data))
          }
          else {
            var.size <- c(var.size, nrow(get(search()[i])))
          }
          if (i == 1 | is.null(attr(get(search()[i]), 
                                    "var.labels")[attr(get(search()[i]), "names") == 
                                                    substitute(data)])) {
            var.lab <- c(var.lab, " ")
          }
          else {
            var.lab <- c(var.lab, attr(get(search()[i]), 
                                       "var.labels")[attr(get(search()[i]), "names") == 
                                                       substitute(data)])
          }
        }
        a <- cbind(search()[candidate.position], var.order, 
                   var.class, var.size, var.lab)
        dim(a)
        colnames(a) <- c("Var. source ", "Var. order", 
                         "Class  ", "# records", "Description")
        rownames(a) <- rep("", length(candidate.position))
        header <- paste("'", deparse(substitute(data)), "'",
                        " is a variable found in the following source(s):", 
                        "\n", "\n", sep = "")
      }
    }
    else {
      dataset <- data[1,]
      if (is.null(attr(data, "var.labels"))) {
        b <- " "
      }
      else {
        b <- attr(data, "var.labels")
        if (length(b) < length(colnames(data))) {
          options(warn = -1)
        }
      }
      class.a <- rep("", ncol(dataset))
      for (i in 1:ncol(dataset)) {
        class.a[i] <- class(dataset[, i])[1]
      }
      a <- cbind(colnames(dataset), class.a, b)
      colnames(a) <- c("Variable     ", "Class          ", 
                       "Description")
      rownames(a) <- 1:nrow(a)
      header <- paste(attr(data, "datalabel"), "\n",.No.of.observations,nrow(data), "\n")
      options(warn = 0)
    }
  }
  results <- list(table=a, header=header)
  class(results) <- c("info","matrix")
  results
}
#####
print.info <- function(data, ...)
{
  cat(data$header)
  print.noquote(data$table)
}
NULL




#' @encoding UTF-8
#' @title Labels variables
#'
#' @description Labels variables
#' 
#' @param variable the variable to be labeled 
#' @param label the label, a short description text. 
#' @param data the \code{data.frame} where \code{var} is.
#' @param replace is logical. If \code{TRUE}, replaces the original column with the new one with label.
#'
#' @examples
#' data(titanic)
#' 
#' info(titanic)
#' 
#' labelvar(CLASS, "4 categories for CLASS", data = titanic)
#' 
#' info(titanic)
#' 
#' @export
labelvar <-function(variable, label, data, replace=TRUE){
  # Store list of variable labels, 
  #if exist, in a temporary vector
  dataset <- data
  if(any(names(dataset)==as.character(substitute(variable)))){
    if(is.null(attributes(dataset)$var.labels)){
      attributes(dataset)$var.labels <- rep("", length(names(dataset)))
    }
    attributes(dataset)$var.labels[names(dataset)==as.character(substitute(var))] <- label
  }else{
    if(length(variable) != nrow(data)){
      stop(paste("The length of", as.character(substitute(variable)), "is not equal to number of rows of", as.character(substitute(data))))
    }
    old.labels <-attributes(dataset)$variable.labels
    dataset[,ncol(dataset)+1]<- variable
    names(dataset)[length(names(dataset))] <- as.character(substitute(variable))
    if(is.null(old.labels)){
      attributes(dataset)$var.labels <- c(rep("", length(names(dataset))-1),label)
    }else{
      attributes(dataset)$var.labels <- c(old.labels,label)
    }
  }
  if(exists(as.character(substitute(variable)))){
    if(!is.atomic(variable)){
      stop(paste("A non-variable object", as.character(substitute( variable)),"exists in the environment and cannot be labelled.","\n", 
                 " If this variable in the data frame is to be labelled,","\n",
                 " either the non-variable object of this name must be removed before labelling","\n", "\n",
                 paste("   rm(",as.character(substitute( variable)),")",";             ",
                       " labelvar(", as.character(substitute(variable)),", \"", as.character(substitute(label)),"\")",sep=""),"\n", "\n",
                 " or the variable in the data frame must be prior renamed","\n",  "\n",
                 paste("   ren(", as.character(substitute( variable)),", newname)", "; ",
                       " labelvar(newname,\"", as.character(substitute(label)),"\")", sep=""), "\n"))
    }
    if(length(variable)==nrow(data)){
      dataset[,names(dataset)==as.character(substitute(variable))] <- variable
    }else{
      stop(paste("The length of", as.character(substitute(variable)), "is not equal to number of rows of", as.character(substitute(data))))
    }
  }
  if(replace){
    suppressWarnings(rm(list=as.character(substitute(variable)), pos=1))
  }
  pos = 1 # does nothing just to trick the environment 
  assign(as.character(substitute(data)), dataset, envir = as.environment(pos))
  if(is.element(as.character(substitute(data)), search())){
    if(length(which(search() %in% as.character(substitute(data))))>1){
      warning(paste("\n","There are more than one '", as.character(substitute(data)),"' attached!","\n", sep=""))
    }
    detach(pos=which(search() %in% as.character(substitute(data)))[1])
    attach(dataset, name=as.character(substitute(data)), warn.conflicts = FALSE)
  }
}
NULL




#' @encoding UTF-8
#' @title Show Observations Randomly Drawn from the Data
#' 
#' @description Provide a sly view of the data by randomly draw observations, instead of showing only the first \code{head()} or the last \code{tail()} rows of an object.
#' 
#' @param x A matrix or data.frame object
#' @param n The number of rows to be shown
#'  
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' 
#' 
#' @keywords Tables
#' @examples
#' data(titanic)
#' peek(titanic)
#' 
#' @export
#' 
peek <- function(x, n = 10) {
  if(is.matrix(x) | is.data.frame(x)) {
    rows <- nrow(x)
    print(x[sort(sample(rows, size = n)),])
  } else {
    cat("'peek' only anticipates matrices and data.frames.\n")
  }
}
NULL





#' @encoding UTF-8
#' @title Parallel sum
#' 
#' @description Provides parallel sum like \code{pmin} and \code{pmax} from the base package. The function \code{sum} simply does not help when the objective is to obtain a vector with parallel sum rather than a scalar value.
#' 
#' @param \dots One or more unit objects
#' @param na.rm A logical value \code{TRUE} or \code{FALSE}, the default
#' 
#' @return A vector containing the parallel sum.
#' 
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' 
#' @keywords Misc
#' 
#' @examples 
#' data(us2012)
#' psum(us2012$Obama, us2012$Romney)
#' swingy <-psum(us2012$Obama, us2012$Romney-100)
#' 
#' @export
psum <-
  function(..., na.rm=FALSE) { 
    x <- list(...)
    rowSums(matrix(unlist(x), ncol=length(x)), na.rm=na.rm)
  }
NULL



#' @encoding UTF-8
#' @title Strip white spaces
#' @param x is a character vector.
#' @param delim is the delimiter, default is white spaces \code{" "} 
#' 
# stripWhite(" Daniel   Marcelino   Silva ")
stripWhite <- function(x, delim = " ") {
  gsub("^\\s+|\\s+$", "",
       gsub(sprintf("\\s+[%s]\\s+|\\s+[%s]|[%s]\\s+",
                    delim, delim, delim), delim, x))
}
NULL





#' @encoding UTF-8
#' @title Return a factor variable of age groups
#'
#' @description Return a factor variable using breaks and labels. The function will calculate the
#' age based upon the \code{to} if given, otherwise the \code{age.var} will be used.
#'
#' @param x if a var containing the age already exists.
#' @param from The date of origin, typically birthdate. 
#' @param to The up to date to compute the age.
#' @param breaks The numeric break guide for grouping age.
#' @param labels The labels for the age groups, can also be set to \code{labels=NULL}.
#' 
#' @author Daniel Marcelino <dmarcelino@@live.com>
#'
#' @examples
#' # The age groupings used by IBGE (grandes grupos).
#' # simulate vector with 1000 age values
#' age <- sample(0:100, 1000, replace = TRUE)
#' mean(age); sd(age); 
#' ageGroups(age, breaks = c(0, 14, 64, Inf), labels = NULL )
#' ageGroups(age, breaks = c(0, 14, 64, Inf), 
#' labels = c("<14", "15-64", "65+") )
#' 
#' ibge_brks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, Inf)
#' ibge_lbls = c("0-4", "5-9", "10-14", "15-19", "20-24",
#'		"25-29", "30-34", "35-39", "40-44", "45-49", "50-54", 
#'  	"55-59", "60-64", "65-69", "70+")
#' ageGroups(age, breaks = ibge_brks, labels = ibge_lbls )
#'
#'
#' @export
#'
ageGroups <- function (x = NULL, from, to, breaks, labels) {
    if (is.null(x)) {
        age = elapsed(from, to)
    }
    else {
        age = x
    }
    ans <- cut(age, breaks = breaks, labels = labels)
    class(ans) <- c("SciencesPo", "factor")
    return(ans) 
}
NULL





#' @encoding UTF-8
#' @title Changes NAs in a vector into a given value
#'
#' @description Changes NAs in a vector into a given value
#' @param x the vector
#' @param value the value to be given to the missing value \code{NA}.
#' @seealso \link{rand.imput}.
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @examples
#' v <- sample(c(round(runif(5, 1, 3)), rep(NA, 2)))
#' NAto0(v)
#'
#'@export
NAto0 <- function(x, value = 0){
  x[is.na(x) == TRUE] <- value
  return(x)
}
NULL




#' @encoding UTF-8
#' @title Eliminate NA observations
#'
#' @description Generates two matrices: One with complete observations and the other with all observations.
#' @param data the \code{data.frame}  
#' 
#' @return two matrices.
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @examples
#' id <- 1:10; var1 <- rnorm(10); var2 <- rgamma(10, 2, 1);
#' df <- data.frame(cbind(id, var1, var2))
#' df[c(5, 9), 3] <- NA
#' eliminateNA(df)
#' 
#'@export
eliminateNA <- function(data){
  rows <- dim(data)[1]
  cols <- dim(data)[2]
  tmp <- matrix(NA, ncol = cols, nrow = rows)
  for (i in 1:cols){tmp[, i] <- as.numeric(data[, i])}
  compl <- data[complete.cases(data) == TRUE, ]
  incompl <- data[complete.cases(data) == FALSE, ]
  ans <- list(complete = compl, incomplete = incompl)
  return(ans)
}
NULL




#' @encoding UTF-8
#' @title Return Elapsed Time in Years
#'
#' @description Return the elapsed time in years.
#'
#' @param from the date of origin, typically birthdate
#' @param to the date up to compute the age
#' @param format the date format see \code{as.Date}
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#'
#' @keywords Data Manipulation
#' @keywords Descriptive Stats
#
#' @examples
#' elapsed(from="1988-12-19", to="2014-12-31", format="%Y-%m-%d")
#'
#' elapsed("1jan1960", "2jan1990", "%d%b%Y")
#'
#' @export
#'
elapsed <- function (from, to, format) {
  round(as.numeric((as.Date(to, format=format) - as.Date(from, format=format))/365.25),1)
}
NULL



#' @encoding UTF-8
#' @title Row Sample
#' @description Sample rows of a data object.
#' @param .data the data object.
#' @param n the number of items to choose from.
#' @author Daniel Marcelino \email{dmarcelino@@live.com}
#' @examples
#' rowSample(iris, 20)
#' @export
rowSample <- function(.data, n) {
  .data[sample(1:nrow(.data), n, replace=FALSE), ] 
}
NULL



#' @encoding UTF-8
#' @title Places quotation marks
#'
#'@param vec the vector whose values will be surounded by quotes 
#' 
#' @examples 
#' x <- 1
#' quotize(x)
#' noquote(quotize(x))
#' a <- ("Daniel")
#' noquote(quotize(a))
#' 
#'@export 
quotize <- function(vec){
  sapply(vec, function(x) paste("'",x,"'",sep=''))}
NULL





#' @encoding UTF-8
#' @title Functions for teaching linear algebra.
#'
#' @description These functions provide a formula based interface to the construction
#' of matrices from data and for fitting.  You can use them both for numerical vectors
#' and for functions of variables in data frames.
#' These functions are intended to support teaching basic linear algebra
#' with a particular connection to statistics.
#'
#' @rdname linearAlgebra
#' @name linearAlgebra
# @aliases mat singvals dot
#'
#' @param formula a formula as \code{~ a or ~ a + b}.  In \code{mat} and \code{singvals},
#' only the right-hand side is used.
#' 
#' @param data a data frame from which to pull out numerical values
#' for the variables in the formula
#'
#' @param \dots additional arguments (currently ignored)
#'
#' 
#' \code{mat} returns a model matrix
#' 
#' To demonstrate singularity, use \code{singvals}.
#' 
#' @return \code{mat} returns a matrix 
#'
#' @examples
#' a <- c(1,0,0); b <- c(1,2,3); c <- c(4,5,6); x <- rnorm(3)
#' # Formula interface
#' mat(~a+b)
#' mat(~a+b+1)
#' 
#' data(tobaccovote)
#' mat(~votedpro+party, data=tobaccovote)
#' singvals(~votedpro*party*money*acres, data=tobaccovote)
#'
#' @export
mat <- function(formula, data=parent.frame()) {
  if( class(formula) != "formula" ) stop("Must provide a formula, e.g., ~ a or ~ a + b ")
  xformula <- update(formula, ~-1+.) # kill off automatic Intercept term
  if( is.null(data) )
    mat <- model.matrix( xformula)
  else
    mat <- model.matrix(xformula, data=data ) 
  
  attr(mat, "assign") <- NULL
  rownames(mat) <- NULL
  
  return(mat)
}

#' @rdname linearAlgebra
#' @return \code{singvals} gives singular values for each column in the model matrix
#' @export
singvals <- function(formula, data=parent.frame()){
  mat <- mat(formula, data=data)
  # formulated to give one singular value for each column in A
  svs <- La.svd(mat, nv=ncol(mat), nu=ncol(mat))$d;
  c( svs, rep(0, ncol(mat) - length(svs)));
}
NULL





#' @encoding UTF-8
#' @title Factor cross products
#' 
#' @description Construct a product of factors.
#'
#' @param \dots  factors to be crossed.
#' @param sep  separator between levels
#' @param drop.unused.levels should levels that do not appear in cross product be dropped?
#' 
#' @return a factor
#' 
#' @examples
#' x <- letters[1:3]
#' y <- c(1,2,1,1,3,1,3)
#' cross(x, y)
#' cross(x, y, drop.unused.levels=TRUE)
#' 
#' @keywords manipulate 
#' @export

cross <- function(..., sep=":", drop.unused.levels=FALSE) {
  factors <- list(...)
  factors <- lapply( factors, function(x) { as.factor(x) } )
  if ( length(factors) < 1 ) {
    stop('No factors specified.')
  }
  levelsList <- lapply(factors, levels)
  
  result <- factors[[1]]
  levels <- levels(result)
  factors[[1]] <- NULL
  while( length(factors) > 0 ) {
    levels <- as.vector( 
      outer (levels(factors[[1]]), levels, function(x,y) { paste(y,x,sep=sep) } ) 
    )
    if (drop.unused.levels ) {
      result <- factor( paste( result, factors[[1]], sep=sep))
    } else {
      result <- factor( paste( result, factors[[1]], sep=sep), levels=levels)
    }
    factors[[1]] <- NULL
  }
  return(result)
}
NULL






#' @encoding UTF-8
#' @title Conditionally convert vectors to factors
#' 
#' @description A generic function and several instances for creating factors from
#' other sorts of data. The primary use case is for vectors that contain
#' few unique values and might be better considered as factors. When
#' applied to a data frame, this is applied to each column in the data.frame.
#' 
#' @param x an object.
#' @param max.levels an integer that determines the number of unique values to be coverted. Default is \code{max.levels = 10}.
#' @param ... additional arguments (currently ignored)
#' 
#' @examples
#' #Some data
#' ID = 1:10
#' Age = round(rnorm(10,50,1))
#' diag = c("Depression","Bipolar");
#' Diagnosis = sample(diag, 10, replace=TRUE)
#' data = data.frame(ID, Age, Diagnosis)
#' factorize(data$Diagnosis)
#' str(factorize(data))
#' @export
factorize <- function(x,  ...) {
  UseMethod("factorize")
}

#' @rdname factorize
#' @export
factorize.default <- function(x, ...) {
  x
}

#' @rdname factorize
#' @export
factorize.numeric <- function(x, max.levels = 10L, ...){
  if (length(unique(x)) <=  max.levels) return ( factor(x, levels=sort(unique(x))) )  
  x
}

#' @rdname factorize
#' @export
factorize.character <- function(x, max.levels = 10L, ...){
  if (length(unique(x)) <=  max.levels) return ( factor(x, levels=sort(unique(x))) )  
  x
}

#' @rdname factorize
#' @export
factorize.data.frame <- function(x, max.levels=10L, ...) {
  as.data.frame( lapply(x, factorize, max.levels=max.levels) )
}
NULL





#' @encoding UTF-8
#' @title Make Data Anonymous
#' 
#' @description This function replaces factor and character variables by a combination of letters and numbers, and numeric columns are also transformed.
#' 
#' @param x A vector or a data frame
#' 
#' @param keep.names A logical argument. If \code{FALSE}, variable names will be replaced by Xs
#' 
#' @details By making difficult to recognize the original data while keeping the same data structure, this function is  quite useful for sharing data on help lists.
#'
#' @return An object of the same type as \code{x}
#' 
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#'
#' @examples
#' # setup data
#' data(ssex)
#' anonymize(ssex)
#' anonymize(ssex, keep.names=FALSE)
#'
#' @keywords Tables
#'
#' @export
#'
anonymize <-
  function(x, keep.names=TRUE){
    truenames <- names(x)
    if(length(x)>26){
      # letters <-replicate(floor(length(x)/26),{letters <-c(LETTERS, paste(LETTERS, LETTERS, sep=""))})
    }
    names(x)<-paste(sample(letters[1:length(x)]))
    level.x<-function(x){
      level.obs<-function(i){
        if(class(x[,i])=="factor" | class(x[,i])=="character"){
          var <-paste(names(x)[i],as.numeric(as.factor(x[,i])), sep="")
        }else if(is.numeric(x[,i])){
          var <-x[,i]- mean(x[,i], na.rm=T)}else{var<-x[,i]}
        return(var)
      }
      x <- data.frame(sapply(seq_along(x), level.obs))
      
      if(keep.names==TRUE){
        names(x) <- truenames 
      }else{
        names(x) <- names(x)
      }
      return(x)
    }
    x <-level.x(x)
    return(x)
  }
NULL





#' @title Filling in missing values
#'
#' \code{fillNA} uses values of a variable from one data set to fill in missing values in another.
#' 
#' @param x the data frame with the variable you would like to fill in.
#' @param y the data frame with the variable you would like to use to fill in \code{D1}.
#' @param xvar a character string of the name of the variable in \code{D1} you want to fill in.
#' @param yvar an optional character string of variable name in \code{D2} that you would like to use to fill in.
#' @param KeyVar a character vector of variable names that are shared by \code{D1} and \code{D2} that can be used to join the data frames.
#' 
#' @examples 
#' # Create data set with missing values
#' naDF <- data.frame(a = sample(c(1,2), 100, rep=TRUE), 
#'                    b = sample(c(3,4), 100, rep=TRUE), 
#'                    fNA = sample(c(100, 200, 300, 400, NA), 100, rep=TRUE))
#'
#' # Created full data set
#' fillDF <- data.frame(a = c(1,2,1,2), 
#'                      b = c(3,3,4,4),
#'                      fFull = c(100, 200, 300, 400))
#'
#' # Fill in missing f's from naDF with values from fillDF
#' Filled <- fillNA(naDF, fillDF, xvar = "fNA", yvar = "fFull", KeyVar = c("a", "b"))
#' @importFrom data.table data.table
#' @importFrom data.table  := 
#' @export
fillNA  <- function(x, y, xvar, yvar = NULL, KeyVar)
{
  # Give Var2 the same name as var1 if Var2 is NULL
  if (is.null(yvar)){
    yvar <- xvar
  } else {
    yvar <- yvar
  }
  VarGen <- VarGen.1 <- NULL
  
  # Give var a generic name
  names(x)[match(xvar, names(x))] <- "VarGen"
  names(y)[match(yvar, names(y))] <- "VarGen.1"
  
  # Convert data frames to data.table type objects
  xTemp <- data.table(x, key = KeyVar)
  yTemp <- data.table(y, key = KeyVar)
  
  # Merge data.tables
  OutDT <- yTemp[xTemp]
  
  # Tell the user how many values will be filled in
  SubNA <- OutDT[, list(VarGen, VarGen.1)]
  SubNA <- subset(SubNA, is.na(VarGen) & !is.na(VarGen.1))
  print(paste(nrow(SubNA), "NAs were replaced."))
  
  # Fill in missing values from D1 with values from D2
  OutDT <- OutDT[is.na(VarGen), VarGen := VarGen.1]
  
  # Convert back to data frame
  OutDF <- data.frame(OutDT)
  
  # Tell the user what the correlation coefficient is between the variables
  SubNoNA <- subset(OutDF, !is.na(VarGen) & !is.na(VarGen.1))
  HowMany <- nrow(SubNoNA)
  CORR <- cor(SubNoNA$VarGen, SubNoNA$VarGen.1, use = "complete.obs")
  print(paste("The correlation between", xvar, "and", yvar, "is", round(CORR, digits = 3), "based on", HowMany, "shared observations." ))
  
  # Remove uncombined variable and return main variable's name
  names(OutDF)[match("VarGen", names(OutDF))] <- xvar
  Keepers <- setdiff(names(OutDF), "VarGen.1")
  OutDF <- OutDF[, Keepers]
  OutDF
}
NULL





#' @title Fill NA by Previous Value
#' @description Fill NA by Previous Value
#' @param column the column that contains NAs
#' @examples
#' data(ssex)
#' peek(ssex)
#' 
#' fillNAByPreviousData(ssex$Favor)
#' 
#' @export
fillNAByPreviousData <- function(column) {
  navals <- which(is.na(column))
  filledvals <- which(! is.na(column))  
  # If there would be no NAs following each other, navals-1 would give the
  # entries we need. In our case, however, we have to find the last column filled for
  # each value of NA. We may do this using the following sapply trick:
  fillup <- sapply(navals, function(x) max(filledvals[filledvals < x]))
  # And finally replace the NAs with our data.
  column[navals] <- column[fillup]
  column
}
NULL





#' @encoding UTF-8
#' @title Make a data.frame Rectangular by filling missing records
#' 
#' @description This function produces a complete rectangularization by adding observations with missing data so that all combinations (interactions) of the specified variables exist.
#' 
#' @param x a data frame.
#' @param by a vector of at least 2 variables from the data frame. If missing all variables in the data frame will be used.
#' @param fill the value used to fill in, default is \code{fill = 0}.
#' 
#' @return a data object of the same class as \code{x}.
#'  
#' @examples
#' data <- data.frame(sex=c("female","male","male"), 
#' race = c("black","black","white"), y = c(.5,.4,.1), x = c(32,40,53))
#' 
#' fillin(data, by=c(sex,race))
#' 
#' fillin(data, by=c(sex,race), fill=0)
#' 
#' @export
fillin <- function(x, by, fill=NA)
{
  Freq <- NULL
  if(missing(by)) by=1:ncol(x)
  nl <- as.list(1:ncol(x))
  names(nl) <- names(x)
  vars <- eval(substitute(by), nl, parent.frame())            
  xt <- data.frame(table(x[,vars]))
  x0 <- subset(xt, Freq==0)[,-length(xt)]
  
  if(nrow(x0)==0){
    x
    warning("Nothing to fill")}
  else{
    z <- as.data.frame(x[1:nrow(x0), -vars, drop=FALSE])
    if(dim(z)[2]==1)
      names(z) <- names(x)[-vars]
    z[,] <- fill
    rbind(x, cbind(x0, z))
  }
}
NULL





#' @encoding UTF-8
#' @title Find the Values Around a Particular Value
#' 
#' @description Find the location of values around a specified value
#' 
#' @param x A vector.
#' @param value Specified value
#' 
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' 
#' @return lo The maximum value of x that is less than or equal to the value parameter.
#' @return hi The minimum value of x that is greater than or equal to the value parameter.
#' 
#' @examples
#' set.seed(123)
#' x = rnorm(25, 5, 10)
#' value = 9
#' around(x, value)
#' 
#' 
#' @export 
around<-function(x, value){
  x<-sort(x)
  lo<-x[nearest.loc(x, value)]
  if(lo>=value)
    lo<-x[nearest.loc(x, value)-1]
  
  hi<-x[nearest.loc(x, value)]
  if(hi<value)
    hi<-x[nearest.loc(x, value)+1]
  
  c(lo, hi)
}
NULL




#' @encoding UTF-8
#' @title Find Location of the Nearest Value
#' 
#' @description Find the location of the nearest value to a number that you specify.
#' 
#' @param x A vector.
#' @param value The value that you want to find.
#' 
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @export
nearest.loc <- function(x, value){
  which(abs(x - value) == min(abs(x - value)))
}
NULL




#' @encoding UTF-8
#' @title Find the Nearest Value
#'
#' @description Find the the nearest value to a number that you specify.
#' @param x A vector.
#' @param value The value that you want to find.
#' 
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @export
nearest <- function(x, value){
  nearloc <- nearest.loc(x, value)
  x[nearloc]
}
NULL




#' @encoding UTF-8
#' @title Writes a delimited text file
#'
#' @description Writes a delimited text file, using tab as seperator.
#'
#' @param data the data object to write the csv.
#' @param name the filename to be stored.
#' @param quote If \code{TRUE}, any character or factor columns will be surrounded by double quotes. Default is \code{quote=FALSE}
#' @param row.names if \code{TRUE}, the row names will be write to file. Default is \code{ row.names=FALSE}.
#' @param sep the field separator string.
#' @param \dots other uncommon arguments to write.table (ex: fileEncoding)
#' 
#' @author Daniel Marcelino \email{dmarcelino@@live.com}.
#'@examples
#'df = data.frame(id=1:20, x=rnorm(20, mean=2, sd=.5), y=rnorm(20, mean=5, sd=2))
#' \dontrun{exportData(df, "MyData")}
#'
#' @export
exportData <- function(data, name, quote=FALSE, row.names=FALSE, sep='\t', ...) {
  filename = paste(name,"_", Sys.Date(),".txt", sep="")
  source <- data
  # write a tab separated tsv, use tab as seperator
  write.table(source, filename, quote=quote, row.names=row.names, col.names=TRUE, sep=sep, ...)
}
NULL




#' @encoding UTF-8
#' @title Converts into percentile
#' @description Converts numeric vector into its percentile.
#' @param x a numeric vector.
#' @author Daniel Marcelino \email{dmarcelino@@live.com}
#' @examples
#' vec <- seq(1:5)
#' percentile(vec)
#' @export
percentile <-
function (x) 
{
  pt1 <- quantile(x, probs = seq(0, 1, by = 0.01), type = 7)
  pt2 <- unique(as.data.frame(pt1), fromLast = TRUE)
  pt3 <- rownames(pt2)
  pt4 <- as.integer(strsplit(pt3, "%"))
  ans <- pt4[as.integer(cut(x, c(0, pt2$pt1), labels = 1:length(pt3)))]
  return(ans)
}
NULL




#' @title Finds ID combination
#' 
#' @description Finds unique id combination
#' 
#' @param columns columns to combine 
#' @param data the data object
#' @param verbose if \code{TRUE} messages maybe be displayed.
#' 
#' @details The original code published at \url{https://stackoverflow.com/} was berely touched.
#' @importFrom data.table data.table
#' @importFrom data.table copy
#' @importFrom data.table setkey 
#' @importFrom data.table haskey 
#' @importFrom data.table is.data.table
#' 
#' @export
isid <- function(columns, data, verbose  = TRUE){
  if(!is.data.table(data)){
    copyd <- data.table(data)
  } else{ 
    copyd <- copy(data)
  }
  if(haskey(copyd)){
    setkey(copyd, NULL)
  }
  # NA values don't work in keys for data.tables
  any.NA <- Filter(columns, f= function(x) any(is.na(copyd[[x]])))
  if(verbose){
    for(aa in seq_along(any.NA)){message(sprintf('Column %s contains NA values', any.NA[aa] ))}
  }
  validCols <- setdiff(columns, any.NA)
  # cycle through columns 1 at a time
  ncol <- 1L
  validKey <- FALSE
  while(!isTRUE(validKey) && ncol <= length(validCols)){
    anyValid <- combn(x = validCols, m = ncol, FUN = function(xn){
      subd <- copyd[, xn, with = FALSE]
      result <- nrow(subd) == nrow(unique(subd))
      list(cols = xn, valid = result)
    }, simplify = FALSE)
    
    whichValid <- sapply(anyValid, `[[`, 'valid')
    validKey <- any(whichValid)
    ncol <- ncol + 1L
  }
  
  if(!validKey){
    warning('No combinations are unique')
    return(NULL)} else {
      valid.combinations <- lapply(anyValid, `[[`, 'cols')[whichValid]
      if(length(valid.combinations) > 1){
        warning('More than one combination valid, returning the first only')
      }
      return(valid.combinations[[1]])
    }
}
NULL




#' @title Detect Outliers
#' 
#' @description Perform an exploaratory test to detect \emph{outliers}. This function returns the minimum and maximum values, respectively preceded by their positions in the \code{vector}, \code{matrix} or \code{data.frame}. The quantity for \emph{min} reveals the minimum deviation from the mean, the integer in \emph{closest} highlights the position of the element. In the same vein, the quantity for \emph{max} is the maximum deviation from the mean, and the \code{farthest} integer is the position of such higher quantity.
#' 
#' @param x A numeric object
#' @param index A numeric value to be considered in the computations
#' 
#' @return The returning object will depend on the inputing object, either a vector or a data frame.
#' 
#' @references Dixon, W.J. (1950) Analysis of extreme values. \emph{Ann. Math. Stat.} \bold{21(4),} 488--506.
#' 
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' 
#' @seealso \link{winsorize} for diminishing the impact of outliers.
#' 
#' @examples
#' outliers(x <- rnorm(20))
#' 
#' #data frame:
#' data(ssex)
#' outliers(ssex)
#' 
#' @export
#' 
outliers <-
  function(x, index=NULL) {
    if (is.data.frame(x)) {
      as.data.frame(sapply(x, outliers, index))
    } else if (is.matrix(x)) {
      apply(x, 2, outliers, index)
    } else if (is.list(x)) {
      lapply(x, outliers, index)
    } else if (is.vector(x)) {
      if (!is.null(index)) {
        if (!is.list(index)) {
          index <- list(index) # make sure index is a list
        }
        unsplit(outliers(split(x,index),index=NULL),index)
      } else {
        
        mu <- mean(x)
        dev <- abs(x - mu)
        closest <- which.min(dev)
        farthest <- which.max(dev)
        min <- dev[closest]
        max <- dev[ farthest]
        output <- data.frame(closest, min, farthest, max)
        return(output)
      }
    } else {
      cat("non-numeric argument to 'outlier'",class(x),"\n",sep="")
    }
  }
NULL




