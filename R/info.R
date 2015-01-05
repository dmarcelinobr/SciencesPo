#' @title Get Information on Data Objects 
#'
#' @param x the data frame to be detailed.
#' @param show the selection of columns from \code{x}, if not all.
#' @param ignore columns from \code{x} to prevent of showing.
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
#' @export
info <- function (x = .data, show, ignore) 
{
    if (!missing(show) | !missing(ignore)) {
        nl <- as.list(1:ncol(x))
        names(nl) <- names(x)
        if (!missing(show)) 
            vars.shown <- eval(substitute(show), nl, parent.frame())
        if (!missing(ignore)) 
            vars.ignored <- eval(substitute(ignore), nl, parent.frame())
        if ((length(grep(pattern = "[*]", as.character(substitute(show)))) == 
            1) | (length(grep(pattern = "[?]", as.character(substitute(show)))) == 
            1)) {
            vars.shown <- grep(pattern = glob2rx(as.character(substitute(show))), 
                names(x))
            if (length(vars.shown) == 0) {
                stop(paste(show, "not matchable with any variable name."))
            }
        }
        if ((length(grep(pattern = "[*]", as.character(substitute(ignore)))) == 
            1) | (length(grep(pattern = "[?]", as.character(substitute(ignore)))) == 
            1)) {
            vars.ignored <- grep(pattern = glob2rx(as.character(substitute(ignore))), 
                names(x))
            if (length(vars.ignored) == 0) {
                stop(paste(ignore, "not matchable with any variable name."))
            }
        }
        vars <- 1:ncol(x)
        if (exists("vars.shown")) 
            vars <- vars[vars.shown]
        if (exists("vars.ignored")) 
            vars <- vars[-vars.ignored]
x1 <- x[1,]
        class.a <- rep("", length(vars))
        for (i in 1:length(vars)) {
            class.a[i] <- class(x1[,vars[i]])[1]
        }
        if (is.null(attr(x, "var.labels"))) {
            a <- cbind(colnames(x1)[vars], class.a, rep("", 
                length(vars)))
        }
        else {
            a <- cbind(colnames(x1)[vars], class.a, attr(x, 
                "var.labels")[vars])
        }
        colnames(a) <- c("Variable     ", "Class          ", 
            "Description")
        rownames(a) <- vars
header <- paste(attr(x, "datalabel"), "\n",.No.of.observations,nrow(x), "\n")
        options(warn = 0)
    }
    else {
        if (!is.data.frame(x)) {
            if (is.character(x) & (length(grep(pattern = "[*]", 
                x)) == 1) | (length(grep(pattern = "[?]", x) == 
                1))) {
                vars <- grep(pattern = glob2rx(x), names(.data))
                if (length(vars) == 0) {
                  stop(paste(x, "not matchable with any variable name."))
                }
                
x1 <- .data[1,]
                class.a <- rep("", length(vars))
                for (i in 1:length(vars)) {
                class.a[i] <- class(x1[,vars[i]])[1]
                }
                if (is.null(attr(.data, "var.labels"))) {
                  a <- cbind(colnames(x1)[vars], class.a, 
                    rep("", length(vars)))
                }
                else {
                  a <- cbind(colnames(x1)[vars], class.a, 
                    attr(.data, "var.labels")[vars])
                }
                colnames(a) <- c("Variable     ", "Class          ", 
                  "Description")
                rownames(a) <- vars
header <- paste(attr(x, "datalabel"), "\n",.No.of.observations,nrow(x), "\n")
                options(warn = 0)
            }
            else {
                candidate.position <- NULL
                for (search.position in 1:length(search())) {
                  if (exists(as.character(substitute(x)), where = search.position)) {
                    if (any(names(get(search()[search.position])) == 
                      as.character(substitute(x))) | any(ls(all.names = TRUE, 
                      pos = 1) == as.character(substitute(x)))) 
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
                    var.order <- c(var.order, which(as.character(substitute(x)) == 
                      names(get(search()[i]))))
                  }
                  if (i == 1) {
                    var.class <- c(var.class, class(x))
                  }
                  else {
                    var.class <- c(var.class, class(get(search()[i])[, 
                      which(as.character(substitute(x)) == names(get(search()[i])))]))
                  }
                  if (i == 1) {
                    var.size <- c(var.size, length(x))
                  }
                  else {
                    var.size <- c(var.size, nrow(get(search()[i])))
                  }
                  if (i == 1 | is.null(attr(get(search()[i]), 
                    "var.labels")[attr(get(search()[i]), "names") == 
                    substitute(x)])) {
                    var.lab <- c(var.lab, " ")
                  }
                  else {
                    var.lab <- c(var.lab, attr(get(search()[i]), 
                      "var.labels")[attr(get(search()[i]), "names") == 
                      substitute(x)])
                  }
                }
                a <- cbind(search()[candidate.position], var.order, 
                  var.class, var.size, var.lab)
                dim(a)
                colnames(a) <- c("Var. source ", "Var. order", 
                  "Class  ", "# records", "Description")
                rownames(a) <- rep("", length(candidate.position))
                header <- paste("'", deparse(substitute(x)), "'",
                " is a variable found in the following source(s):", 
                  "\n", "\n", sep = "")
            }
        }
        else {
x1 <- x[1,]
            if (is.null(attr(x, "var.labels"))) {
                b <- " "
            }
            else {
                b <- attr(x, "var.labels")
                if (length(b) < length(colnames(x))) {
                  options(warn = -1)
                }
            }
            class.a <- rep("", ncol(x1))
            for (i in 1:ncol(x1)) {
                class.a[i] <- class(x1[, i])[1]
            }
            a <- cbind(colnames(x1), class.a, b)
            colnames(a) <- c("Variable     ", "Class          ", 
                "Description")
            rownames(a) <- 1:nrow(a)
header <- paste(attr(x, "datalabel"), "\n",.No.of.observations,nrow(x), "\n")
            options(warn = 0)
        }
    }
results <- list(table=a, header=header)
class(results) <- c("info","matrix")
results
}
#####
print.info <- function(x, ...)
{
cat(x$header)
print.noquote(x$table)
}