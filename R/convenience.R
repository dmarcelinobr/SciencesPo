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
#' @param from The date of origin, typically birthdate 
#' @param to The up to date to compute the age
#' @param breaks The numeric break guide for grouping age
#' @param labels The labels for the age groups, can also be set to \code{labels=NULL}
#' @param age.var If an age var is present
#' 
#' @author Daniel Marcelino <dmarcelino@@live.com>
#'
#' @examples
#' # The age groupings used by IBGE (grandes grupos).
#' x <- sample(100)
#' ageGroups(age.var = x, breaks = c(0, 14, 64, Inf), labels = NULL )
#' ageGroups(age.var = x, breaks = c(0, 14, 64, Inf), 
#' labels = c("<14", "15-64", "65+") )
#' 
#' 
#' ibge_brks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, Inf)
#' ibge_lbls = c("0-4", "5-9", "10-14", "15-19", "20-24",
#'		"25-29", "30-34", "35-39", "40-44", "45-49", "50-54", 
#'  	"55-59", "60-64", "65-69", "70+")
#' ageGroups(age.var = x, breaks = ibge_brks, labels = ibge_lbls )
#'
#'
#' @export
#'
ageGroups <- function (from, to, breaks, labels,
		age.var = NULL) {
    if (is.null(age.var)) {
        age = elapsed(from, to)
    }
    else {
        age = age.var
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
#' @seealso \code{\link{complete.cases}}
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



#' @title Writes a delimited text file
#'
#' @param .data the data frame to be written 
#' @param file the name of the file
#' @param quote If \code{TRUE}, any character or factor columns will be surrounded by double quotes. Default is \code{quote=FALSE}.
#' @param row.names if \code{TRUE}, the row names will be write to file. Default is \code{ row.names=FALSE}.
#' @param sep the field separator string.
#' @param \dots other uncommon arguments to write.table (ex: fileEncoding).
#'
#'@export
write.delim <- function(.data, file, quote=FALSE, row.names=FALSE, sep='\t', ...){
  write.table(.data, file,  quote=quote, row.names=row.names, sep=sep, ...)
}
NULL




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
#' mat(~SURVIVED+CLASS, data=titanic)
#' singvals(~SURVIVED*CLASS*SEX, data=titanic)
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

#
##################
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




#' Factor cross products
#' 
#' Construct a product of factors.
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




#' Conditionally convert vectors to factors
#' 
#' A generic function and several instances for creating factors from
#' other sorts of data.  The primary use case is for vectors that contain
#' few unique values and might be better considered as factors.  When
#' applied to a data frame, this is applied to each variable in the data frame.
#' 
#' @param x an object
#' @param max.levels an integer.  Only convert if the number of unique values is no 
#' more than \code{max.levels}.
#' @param ... additional arguments (currently ignored)
#' 
#' @export
#' @examples
#' #Some data
#' ID=1:10
#' Age=round(rnorm(10,50,1)) #round makes the ages, with mean of 50 sd 1, whole numbers
#' diag=c("Depression","Bipolar");Diagnosis=sample(diag,10,replace=TRUE)
#' data=data.frame(ID,Age,Diagnosis)

#' factorize(data$Diagnosis)
#' str(factorize(data))

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
factorize.numeric <- function(x, max.levels = 5L, ...){
  if (length(unique(x)) <=  max.levels) return ( factor(x, levels=sort(unique(x))) )  
  x
}

#' @rdname factorize
#' @export
factorize.character <- function(x, max.levels = 5L, ...){
  if (length(unique(x)) <=  max.levels) return ( factor(x, levels=sort(unique(x))) )  
  x
}

#' @rdname factorize
#' @export
factorize.data.frame <- function(x, max.levels=5L, ...) {
  as.data.frame( lapply(x, factorize, max.levels=max.levels) )
}

