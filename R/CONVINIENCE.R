#' @encoding UTF-8
#' @title Make a data.frame Rectangular by fill in missing records
#'
#' @description \code{rfill} produces a complete rectangularization table by adding observations with missing data so that all combinations (interactions) of the specified variables exist.
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
#' retangular.fill(data, by=c(sex,race))
#'
#' retangular.fill(data, by=c(sex,race), fill=0)
#'
#' @export
`retangular.fill` <- function(x, by, fill=NA)
{
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
#' @title Attach exclusively various file formats
#'
#' @description This works rigorously as the \pkg{epicalc}'s \code{use} function, though limited for the file formats it can read. Fundamentally, it replaces the command attach of R and save an object with extension \code{.data}, which becomes the default dataset. All other \code{data.frames} will be detached, by the time of using the \code{use} function, unless the argument \code{clear = FALSE} is specified.
#'
#' @param file The name of the file which the data are to be read from.
#' @param data The internal name after attaching the data file.
#' @param clear If \code{clear = TRUE}, all attached data in the environment will be detached first.
#' @param spss.missing Whether SPSS missing values should be replaced with NA; default is \code{spss.missing = TRUE}.
#' @param tolower Whether variable names should be forced to lower case; default is \code{tolower = TRUE}.
#'
#' @details By using this \dQuote{attach} version, the data becomes available globally usually positioned in the second place, \code{search()}.
#'
#' @importFrom foreign read.dta
#' @importFrom foreign read.spss
#' @importFrom foreign read.dbf
#'
#' @examples
#' use(ssex)
#'
#' @export
`use` <-
  function (file, data = .data, clear = TRUE, spss.missing = TRUE, tolower = TRUE)
  {
    if (clear) {
      detach.all()
    }
    if (is.character(file)) {
      ext <- tolower(substring(file, first = nchar(file) -
                                 3, last = nchar(file)))
      if (ext == ".dta") {
        data1 <- read.dta(file)
      }
      else {
        if (ext == ".dbf") {
          data1 <- read.dbf(file)
          if (tolower)
            names(data1) <- tolower(names(data1))
        }
          else {
            if (ext == ".sav") {
              data0 <- read.spss(file)
              var.labels <- attr(data0, "variable.labels")
              data1 <- read.spss(file, to.data.frame=TRUE, trim.factor.names=TRUE)
              data1 <- data1[1:nrow(data1), 1:ncol(data1)]
              attr(data1, "var.labels") <- var.labels
              if(spss.missing){
                for(i in 1:ncol(data1)){
                  if(!is.null(attr(data0, "missing")[[i]]$value)){
                    data1[,i] <- ifelse((data1[,i] %in% attr(data0, "missing")[[i]]$value),NA,data1[,i])
                  }
                  if(!is.null(attributes(data0[[i]])$value.labels)){
                    data1[,i] <- ifelse((data1[,i] %in% attributes(data0[[i]])$value.labels),NA,data1[,i])
                  }
                }
              }
              if (tolower)
                names(data1) <- tolower(names(data1))
            }
            else {
              if (substring(file, first = nchar(file) -
                            3, last = nchar(file)) == ".csv") {
                data1 <- read.csv(file, header = TRUE,
                                  sep = ",")
              }
              else {
                stop("This type of file cannot be 'use'd.")
            }
          }
        }
      }
    }
    else {
      if (is.data.frame(file)) {
        data1 <- file
      }
      else {
        stop("The argument is not a data frame or no such file")
      }
    }
    nr <- nrow(data1);
    nc <- ncol(data1);
    assign(as.character(substitute(data)), data1, pos = 1)
    attach(data1, name = as.character(substitute(data)),
           warn.conflicts = FALSE)
    message(paste0('[', nr, " x ", nc, ']', " assigned to `.data`", sep=""));
    }
NULL


#' @encoding UTF-8
#' @title View random n elements of an object.
#'
#' @description Provide a sly view of the data by randomly drawn observations, instead of showing only the first \code{head()} or the last \code{tail()} rows of an object, though it is also possible to constrain for the first/last elements.
#'
#' @param data A matrix or data.frame object
#' @param n The number of rows to be shown
#' @param random If TRUE, observartions are sampled.
#' @param tail If TRUE, will show n lines from tail.
#' @param print.console If TRUE, dada will be printed on console.
#' @param \dots Some additional parameters.
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#'
#'
#' @keywords Tables
#' @examples
#' use(titanic)
#' view(titanic)
#'
#' @export
`view` <- function (data=.data, n=10, random=TRUE, tail=FALSE, print.console=TRUE, ...) {
  getn=function(n,N,tail,random,...) {
    n=min(n,N)
    if (random) return(sample(1:N,n,...))
    n1=ifelse(tail,N-n+1,1); n2=ifelse(tail,N,n)
    return(n1:n2) }
  showVec=function(data,n,tail,random,...){
    N=length(data); if (N==0) return("empty vector")
    v.vec=data[getn(n,N,tail,random,...)]
    return(v.vec) }
  showTab=function(data,n,tail,random,...){
    N=nrow(data); if (N==0) return("empty table")
    mess = paste(c("v.tab=data[getn(n,N,tail,random,...)",rep(",",length(dim(data))-1),"]"),collapse="")
    eval(parse(text=mess))
    return(v.tab) }
  showLis=function(data,n,tail,random,print.console,...){
    nL=length(data); if (nL==0) return("empty list")
    v.lis=list()
    if (is.null(names(data))) ii=1:nL else ii=names(data)
    for (i in 1:nL) {
      iobj=data[[i]]
      v.lis = c(v.lis,list(view(iobj,n,tail,random,print.console=FALSE,...)))
    }
    names(v.lis)=ii; return(v.lis) }
  showAll=function(data){
    return(data) }
  # End Subfunction------------------------------
  if (n==0) return("nada")
  n=abs(n) # coerce to positive
  if (is.data.frame(data) || is.matrix(data) || is.array(data))
    viewed=showTab(data,n,tail,random,...)
  else if (is.list(data))
    viewed=showLis(data,n,tail,random,print.console,...)
  else if (is.vector(data) || is.integer(data) || is.numeric(data) || is.character(data))
    viewed=showVec(data,n,tail,random,...)
  else viewed=showAll(data)
  if (print.console) print(viewed)
  invisible(viewed)

  nr <- nrow(data);
  nc <- ncol(data);
  message(paste0("A kind of ", '[', nr, " x ", nc, ']', " data object.", sep=""));
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
`factorize` <- function(x,  ...) {
  UseMethod("factorize")
}

#' @rdname factorize
#' @export
`factorize.default` <- function(x, ...) {
  x
}

#' @rdname factorize
#' @export
`factorize.numeric` <- function(x, max.levels = 10L, ...){
  if (length(unique(x)) <=  max.levels) return ( factor(x, levels=sort(unique(x))) )
  x
}

#' @rdname factorize
#' @export
`factorize.character` <- function(x, max.levels = 10L, ...){
  if (length(unique(x)) <=  max.levels) return ( factor(x, levels=sort(unique(x))) )
  x
}

#' @rdname factorize
#' @export
`factorize.data.frame` <- function(x, max.levels=10L, ...) {
  as.data.frame( lapply(x, factorize, max.levels=max.levels) )
}
NULL




#' @encoding UTF-8
#' @title Aggregate a numeric variable
#'
#' @param formula The variable versus factor(s) to be computed (y~factor+factor).
#' @param data The data object.
#' @param FUN The function statistic to be calculated.
#' @examples
#' # data:
#' df=data.frame(group=sample(letters,100, TRUE),y=sample(100) )
#'
#' #functions:
#' FUNS <- function(x) c(N=nobs(x), mean=round(mean(x),0),
#' sd=round(sd(x), 0), min=round(min(x),0),
#' max=round(max(x),0))
#'
#' # Do the computation
#' compute(y~group, data=df, FUN=FUNS)
#' @export
compute <- function(formula, data=.data, FUN){
  if(class(FUN)=="list"){
    f <- function(x) sapply(FUN, function(fun) fun(x))
  }else{f <- FUN}
  temp <- aggregate(formula, data, f)
  out <- data.frame(temp[,-ncol(temp)], temp[,ncol(temp)])
  colnames(out)[1] <- colnames(temp)[1]
  return(out)
}
NULL





#' @encoding UTF-8
#' @title Sort data set and related vector
#'
#' @param \dots Index variable(s) used for sorting.
#' @param data The data frame where all variables of the same length are sorted.
#' @param inclusive Whether vectors outside the default data frame should also be sorted.
#'
#' @examples
#' df=data.frame(group=sample(letters,10, TRUE),y=sample(10) )
#' use(df)
#' y2 <- y^2
#' bysort(y, inclusive = FALSE)
#' y2 # unsorted
#' bysort(y, inclusive = TRUE)
#' y2 # sorted
#' bysort(y, decreasing=TRUE)
#' .data
#' @export
bysort <- function(..., data = .data, inclusive=TRUE) {
  data1 <- data
  data1 <- data1[order(...),]
  if(inclusive){
    y <- setdiff(lsNoFunction(), as.character(ls.str(mode="list")[]))
    if (length(y)>0){
      for(i in 1:length(y)){
        if(length(get(y[i]))==nrow(data1)){
          nam <- y[i]
          assign (nam, (get(y[i]))[order(...)], envir = .GlobalEnv)
        }
      }
    }
  }
  detach.all()
  assign(as.character(substitute(data)), data1, pos=1)
  attach(data1, name=as.character(substitute(data)), warn.conflicts = FALSE)
}
NULL






