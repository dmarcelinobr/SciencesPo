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
#' rowSample(iris, 250)
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





#' @title Fetch data from a web service
#'
#' @description Read a data set generated from a web service such as Google Docs.
#' 
#' @param URL the URL to retrieve a CSV file from the service
#' @param key for convenience, just the "key" part of the Google link
#' 
#' @details Generating the URL from the web service will, of course, depend on 
#' how that service is set up.  For Google Spreadsheets, you, the owner of a
#' spreadsheet, can
#' (1) open the spreadsheet in a browser
#' (2) select the File/Publish to the Web menu item
#' (3) in the resulting dialog box, press "Start publishing"
#' (4) under "Get a link to the published data", select CSV format
#' (5) copy the \code{https://docs.google.com/spreadsheet/pub?...} link and post
#' it where your users can get to it.
#' 
#' 
#' @note 
#' The \code{key=} argument is provided as a convenience so that a shorter
#' character string can be used to refer to a Google document.  Use \code{URL} rather than
#' \code{key} if you are using a non-Google service or if the Google interface
#' changes.
#' \code{fetchData()} expects the spreadsheet to be in a straightforward 
#' rectangular spreadsheet format.  
#' 
#' @rdname fetchGoogle
#' @name fetchGoogle
#' @author Daniel Kaplan (\email{kaplan@@macalester.edu})
#' 
#' @examples
#' \dontrun{s = fetchGoogle(key="0Am13enSalO74dEVzMGJSMU5TbTc2eWlWakppQlpjcGc")}
#' @export

fetchGoogle <- function(URL,key=NULL){
  
  if (! requireNamespace("RCurl")) stop("Package `RCurl' must be installed.")
  
  if (missing(URL) & !is.null(key))
    URL = paste("https://docs.google.com/spreadsheet/pub?key=",
                key,"&single=TRUE&gid=0","&output=csv",sep="")
  s = RCurl::getURLContent(URL)
  foo = textConnection(s)
  b = read.csv(foo)
  close(foo)
  return(b)
}
NULL





#' Functions for teaching linear algebra.
#'
#' These functions provide a formula based interface to the construction
#' of matrices from data and for fitting.  You can use them both for numerical vectors
#' and for functions of variables in data frames.
#' These functions are intended to support teaching basic linear algebra
#' with a particular connection to statistics.
#'
#' @rdname linear.algebra
#' @name linear.algebra
# @aliases mat singvals dot
#'
#' @param A a formula.  In \code{mat} and \code{singvals},
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
#' @seealso \code{\link{project}}
#'
# @usage mat(A,data=NULL)
#' @seealso \code{\link{linearModel}}, which returns a function.
#' @examples
#' a <- c(1,0,0); b <- c(1,2,3); c <- c(4,5,6); x <- rnorm(3)
#' # Formula interface
#' mat(~a+b)
#' mat(~a+b+1)
#' if (require(mosaicData)) {
#' mat(~length+sex, data=KidsFeet)
#' singvals(~length*sex*width, data=KidsFeet)
#' }
#' @export

mat <- function(A, data=parent.frame()) {
  if( class(A) != "formula" ) stop("Must provide a formula, e.g., ~ a or ~ a + b ")
  A <- update(A, ~-1+.) # kill off automatic Intercept term
  if( is.null(data) )
    M <- model.matrix( A )
  else
    M <- model.matrix( A, data=data ) 
  
  attr(M, "assign") <- NULL
  rownames(M) <- NULL
  
  return(M)
}

#
##################
#' @rdname linear.algebra
#' @return \code{singvals} gives singular values for each column in the model matrix
#' @export

singvals <- function(A, data=parent.frame()){
  M <- mat(A, data=data)
  # formulated to give one singular value for each column in A
  svs <- La.svd(M, nv=ncol(M), nu=ncol(M))$d;
  c( svs, rep(0, ncol(M) - length(svs)));
}
NULL






#' Lattice Theme
#' 
#' A theme for use with lattice graphics.
#'
#' @param bw whether color scheme should be "black and white"
#' @param lty vector of line type codes
#' @param ... additional named arguments passed to 
#'   \code{\link{trellis.par.set}}
#' 
#' @return Returns a list that can be supplied as the \code{theme} to 
#' \code{\link{trellis.par.set}()}.
#' @note
#'   These two functions are identical.  \code{col.mosaic} is named 
#' similarly to \code{\link[lattice]{col.whitebg}}, but since more 
#' than just colors are set, \code{theme.mosaic} is a preferable name.
#'
#'
#' @seealso \code{\link{trellis.par.set}}, \code{\link{show.settings}} 
#' 
#' @rdname themes
#'
#' @examples
#' trellis.par.set(theme=theme.SciencesPo())
#' show.settings()
#' trellis.par.set(theme=theme.SciencesPo(bw=TRUE))
#' show.settings()
#' 
#' @keywords graphics 
#' @export

theme.SciencesPo <-
  function (bw = FALSE, lty = if (bw) 1:7 else 1, ...) 
  {
    aBlue <- colorRampPalette(c("white", "navy"))(10)[8]
    paleBlue <- colorRampPalette(c("white", "navy"))(10)[6]
    lightBlue <- colorRampPalette(c("white", "steelblue"))(10)[5]
    veryLightBlue <- colorRampPalette(c("white", "steelblue"))(12)[3]
    darkBlue <- colorRampPalette(c("white", "navy"))(10)[9]
    paleGreen <- colorRampPalette(c("white", "darkGreen"))(10)[8]
    if (bw) {
      res <- list(background = list(col = "transparent"), axis.line = list(col = "gray30"), 
                  axis.text = list(col = "gray30"), plot.polygon = list(col = "gray80"), 
                  box.rectangle = 
                    list(col = "gray10"), box.umbrella = list(col = "gray10", 
                                                              lty = 1), box.dot = list(col = "gray10"), dot.line = list(col = "gray50"), 
                  dot.symbol = 
                    list(col = "gray30", pch = 16), plot.line = list(col = "black", 
                                                                     lwd = 2), plot.symbol = list(col = "black", fill = "gray80", 
                                                                                                  pch = 16), regions = list(col = gray((1:100)/100)), 
                  reference.line = 
                    list(col = "gray50"), add.line = list(lty = 1,  col = "gray80", lwd = 2), 
                  superpose.polygon = 
                    list(col = c("gray30", "gray70", "black", "gray50", "gray20", "gray80", 
                                 "gray60", "gray40"), fill = c("gray80")), 
                  superpose.line = 
                    list(lty = lty, lwd = 2, 
                         col = c("gray30", "gray70", "black", 
                                 "gray50", "gray20", "gray80", "gray60", "gray40")), 
                  superpose.symbol = 
                    list(pch = c(16, 15, 18, 1, 3, 6, 0, 5), 
                         cex = rep(0.7, 7), 
                         col = c("gray30","gray70", "black", "gray50", "gray20", "gray80", 
                                 "gray60", "gray40")),
                  par.strip.text = list(cex = 0.5, col = c("gray60", "gray30")) 
      )
    } else {
      res <- list(background = list(col = "transparent"), 
                  plot.polygon = list(col = paleBlue), 
                  superpose.polygon = 
                    list(
                      col = c(aBlue, 
                              "lightskyblue3", "darkgreen", "tan", "orange", 
                              "purple", "lightgreen")), 
                  box.rectangle = 
                    list(col = darkBlue), box.umbrella = list(col = darkBlue), 
                  dot.line = list(col = "#e8e8e8"), 
                  dot.symbol = 
                    list(col = darkBlue, pch = 16), 
                  plot.line = 
                    list(lwd = 2, col = darkBlue), 
                  plot.symbol = 
                    list(col = darkBlue, pch = 16), 
                  regions = list(col = heat.colors(100)), 
                  reference.line =  list(col = "#e8e8e8"), 
                  add.line = list(lty = 1, col = "gray20", lwd = 2), 
                  superpose.line = 
                    list(lty = lty, 
                         col = c(darkBlue, "lightskyblue3", "darkgreen",  
                                 "tan", "orange", "purple", "pink", "lightgreen")), 
                  superpose.symbol = 
                    list(pch = c(16, 15, 18, 1, 3,  6, 0, 5), 
                         cex = rep(0.7, 7), 
                         col = c(darkBlue, "lightskyblue3", "darkgreen", "tan", "orange", 
                                 "purple", "pink", "lightgreen")), 
                  strip.background = 
                    list(alpha = 1, 
                         col = c("#ffe5cc", veryLightBlue, "#ccffff", 
                                 "#cce6ff", "#ffccff", "#ffcccc", "#ffffcc")), 
                  strip.shingle = 
                    list(alpha = 1, 
                         col = c("#ff7f00", 
                                 darkBlue, "#00ffff", "#0080ff", "#ff00ff", "#ff0000", 
                                 "#ffff00")), 
                  par.strip.text = list(cex = 0.5)
      )
    }
    res <- c(res, list(...))
    res
  }

#' @rdname themes
#' @export

col.SciencesPo<- theme.SciencesPo



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
#' diag=c("Depression","Bipolar");Diagnosis=sample(diag,10,replace=T)
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

