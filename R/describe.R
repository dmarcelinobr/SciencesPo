#' @encoding UTF-8
#' @title Get Information on Data Objects 
#'
#' @param data the data frame to be detailed.
#' @param show the selection of columns from \code{data}, if not all.
#' @param ignore columns from \code{data} to prevent of showing.
#' 
#' @examples
#' data(titanic)
#' use(titanic)
#' ## Wildcard for variables
#' info("C*") # Show all variables starting with 'C'
#' ## Subset of variables
#' info(show = CLASS:SEX) # Same results
#' info(show = 1:3)
#' ## Exclusion using wildcard.
#' info(ignore = "C*")
#' 
#' @export
info <- function (data = .data, show, ignore) 
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
data1 <- data[1,]
        class.a <- rep("", length(vars))
        for (i in 1:length(vars)) {
            class.a[i] <- class(data1[,vars[i]])[1]
        }
        if (is.null(attr(data, "var.labels"))) {
            a <- cbind(colnames(data1)[vars], class.a, rep("", 
                length(vars)))
        }
        else {
            a <- cbind(colnames(data1)[vars], class.a, attr(data, 
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
                vars <- grep(pattern = glob2rx(data), names(.data))
                if (length(vars) == 0) {
                  stop(paste(data, "not matchable with any variable name."))
                }
                
data1 <- .data[1,]
                class.a <- rep("", length(vars))
                for (i in 1:length(vars)) {
                class.a[i] <- class(data1[,vars[i]])[1]
                }
                if (is.null(attr(.data, "var.labels"))) {
                  a <- cbind(colnames(data1)[vars], class.a, 
                    rep("", length(vars)))
                }
                else {
                  a <- cbind(colnames(data1)[vars], class.a, 
                    attr(.data, "var.labels")[vars])
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
data1 <- data[1,]
            if (is.null(attr(data, "var.labels"))) {
                b <- " "
            }
            else {
                b <- attr(data, "var.labels")
                if (length(b) < length(colnames(data))) {
                  options(warn = -1)
                }
            }
            class.a <- rep("", ncol(data1))
            for (i in 1:ncol(data1)) {
                class.a[i] <- class(data1[, i])[1]
            }
            a <- cbind(colnames(data1), class.a, b)
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
#' @param var the variable to be labeled 
#' @param label the label, a short description text. 
#' @param drop is logical. If \code{TRUE}, replaces the original column with the new one with label.
#' @param data the \code{data.frame} where \code{var} is.
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
labelvar <-function(var, label, drop=TRUE, data = .data){
  # Store list of variable labels, 
  #if exist, in a temporary vector
  data1 <- data
  if(any(names(data1)==as.character(substitute(var)))){
    if(is.null(attributes(data1)$var.labels)){
      attributes(data1)$var.labels <- rep("", length(names(data1)))
    }
    attributes(data1)$var.labels[names(data1)==as.character(substitute(var))] <- label
  }else{
    if(length(var) != nrow(data)){
      stop(paste("The length of", as.character(substitute(var)), "is not equal to number of rows of", as.character(substitute(data))))
    }
    old.labels <-attributes(data1)$var.labels
    data1[,ncol(data1)+1]<- var
    names(data1)[length(names(data1))] <- as.character(substitute(var))
    if(is.null(old.labels)){
      attributes(data1)$var.labels <- c(rep("", length(names(data1))-1),label)
    }else{
      attributes(data1)$var.labels <- c(old.labels,label)
    }
  }
  if(exists(as.character(substitute(var)))){
    if(!is.atomic(var)){
      stop(paste("A non-variable object", as.character(substitute( var)),"exists in the environment and cannot be labelled.","\n", 
                 " If this variable in the data frame is to be labelled,","\n",
                 " either the non-variable object of this name must be removed before labelling","\n", "\n",
                 paste("   rm(",as.character(substitute( var)),")",";             ",
                       " labelvar(", as.character(substitute(var)),", \"", as.character(substitute(label)),"\")",sep=""),"\n", "\n",
                 " or the variable in the data frame must be prior renamed","\n",  "\n",
                 paste("   ren(", as.character(substitute( var)),", newname)", "; ",
                       " labelvar(newname,\"", as.character(substitute(label)),"\")", sep=""), "\n"))
    }
    if(length(var)==nrow(data)){
      data1[,names(data1)==as.character(substitute(var))] <- var
    }else{
      stop(paste("The length of", as.character(substitute(var)), "is not equal to number of rows of", as.character(substitute(data))))
    }
  }
  if(drop){
    suppressWarnings(rm(list=as.character(substitute(var)), pos=1))
  }
  assign(as.character(substitute(data)), data1, pos=1)
  if(is.element(as.character(substitute(data)), search())){
    if(length(which(search() %in% as.character(substitute(data))))>1){
      warning(paste("\n","There are more than one '", as.character(substitute(data)),"' attached!","\n", sep=""))
    }
    detach(pos=which(search() %in% as.character(substitute(data)))[1])
    attach(data1, name=as.character(substitute(data)), warn.conflicts = FALSE)
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





