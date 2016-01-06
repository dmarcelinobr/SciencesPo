# compatibility for data.table functions
## Look for existing generic functions also in imported namespaces.
## This will affect whether setGenericS3() creates a generic function
## or not.
options("R.methodsS3:checkImports:setGenericS3"=TRUE)

.datatable.aware <- TRUE
setnames <- `names<-`
setclass <- `class<-`

#' @title Chain operator
#' @description Chain operator.
#' @name %>%
#' @importFrom magrittr %>%
#' @export %>%
#' @keywords manipulation
#' @rdname chain
#' @usage x %>% f(y) is translated into f(x, y).
NULL

if (getRversion() >= "2.15.1")
  globalVariables(c(".data", ".dp.main"))


.onAttach <- function(...) {
  # Send message
  msg <- function() {
    #message("")
    packageStartupMessage("initializing ...", appendLF = FALSE)
    Sys.sleep(1)
    packageStartupMessage(" done")
  }
packageStartupMessage(msg())
  #suppressMessages(msg())
options(n.cat=0)
options(quiet=FALSE)
options(brief=FALSE)
  ggplot2::theme_set(theme_pub())
}
NULL


say.hello <- function() {
  print("Hello World!")
}

say.yo <- function() {
  print("Yo world!")
}

"%=%" <- function(x,y) {assign(as.character(substitute(x)), y, envir = parent.frame())}


# useful for avoinding extra space between columns
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}

is.valid.name <- function(x) {
  length(x) == 1 && is.character(x) && x == make.names(x)
}


noathenb <- function(a, b) {
  if (length(a) > 0) a else b
}

"%||%" <- noathenb

naathenb <- function(a, b) {
  if (length(a) > 0) {
    if (!is.na(a)) a else b
  } else {
    b
  }
}

"%^^%" <- naathenb


# Adds extra habilities to the base match.arg function:
`.Match` <- function(arg, choices, base = 1, several.ok = FALSE,
                         numeric = FALSE, ignore.case = TRUE)
{
  if (missing(choices)) {
    formal.args <- formals(sys.function(sys.parent()))
    choices <- eval(formal.args[[deparse(substitute(arg))]])
  }
  if (is.character(arg)) {
    if (ignore.case) {
      choices = tolower(choices)
      arg = tolower(arg)
    }
    res = match.arg(arg=arg,choices=choices,several.ok=several.ok)
    if (numeric)  res = which(choices %in% res) + base - 1
  } else if (is.numeric(arg)) {
    if ( (arg<base) | (arg>(length(choices)+base-1)) )
      stop("'arg' should be between ",base," and ",length(choices)+base-1)
    if (numeric) {
      res = arg
    } else {
      res = choices[arg - base + 1]
    }
  } else stop("'arg' should be numeric or character")
  return(res)
}
NULL


formatBRL<- function(x, digits=2, nsmall=2){
  paste("\u0052\u0024", format(x, digits = digits, nsmall=nsmall, big.mark = ".", decimal.mark = ","))
}

formatEUR<- function(x, digits=2, nsmall=2){
  paste("\u20ac", format(x, digits = digits, nsmall=nsmall))
}

formatUSD<- function(x, digits=2, nsmall=2){
  paste("\u0024", format(x, digits = digits, nsmall=nsmall))
}

formatPercent<- function(x, digits=2, nsmall=2, decimal.mark = ","){
  paste("\u0025", format(x, digits = digits, nsmall=nsmall,  decimal.mark =  decimal.mark ))
}

`user.prompt` <- function (msg = NULL) {
  if (is.null(msg))
    msg <- "Press <return> to continue: "
  msg <- paste("\n", msg, sep="")
invisible(readline(msg))
}
NULL


empty <- function(df) {
  is.null(df) || nrow(df) == 0 || ncol(df) == 0
}

is.discrete <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x)
}

is.formula <- function(x) inherits(x, "formula")


`quotize` <- function(vec){
  sapply(vec, function(x) paste("'",x,"'",sep=''))}
NULL


`hour2min` <- function(hhmm) {
  hhmm <- as.numeric(hhmm)
  trunc(hhmm/100)*60 + hhmm %% 100
}

`min2hour` <- function(min) {
  min <- as.numeric(min)
  trunc(min/60)*100 + min %% 60
}


.max.dd <- function(x) {

 n.dec <-function(xn) {
    xc <- format(xn)  # as.character(51.45-48.98) does not work
    nchar(xc)
    ipos <- 0
    for (i in 1:nchar(xc)) if (substr(xc,i,i)==".") ipos <- i
    n.dec <- ifelse (ipos > 0, nchar(xc)-ipos, 0)
    return(n.dec)
  }

  max.dd <- 0
  for (i in 1:length(x))
    if (!is.na(x[i])) if (n.dec(x[i]) > max.dd) max.dd <- n.dec(x[i])

  return(max.dd)
}



# change class call to class character
.fun.call.deparse <- function(fun.call) {

  fc.d <- deparse(fun.call)
  if (length(fc.d) > 1) {  # multiple lines
    fc <- fc.d[1]
    for (i in 2:length(fc.d)) fc <- paste(fc, fc.d[i], sep="")
  }
  else
    fc <- fc.d

  fc <- sub("     ", " ", fc, fixed=TRUE)
  fc <- sub("    ", " ", fc, fixed=TRUE)
  fc <- sub("  ", " ", fc, fixed=TRUE)

  return(fc)

}


.specifyDecimals <- function(x, k) format(round(x, k), nsmall=k)

.getdigits <- function(x, min.digits) {
  digits.d <- .max.dd(x) + 1
  if (digits.d < min.digits) digits.d <- min.digits
  return(digits.d)
}


.dash <- function(ndash, cc, newline=TRUE) {
  if (missing(cc)) cc <- "-"
  for (i in 1:(ndash)) cat(cc)
  if (newline) cat("\n")
}

.dash2 <- function(ndash, cc="-") {
  tx <- ""
  if (!is.null(cc)) for (i in 1:(ndash)) tx <- paste(tx, cc, sep="")
  return(tx)
}






.fmt <- function(k, d=getOption("digits.d"), w=0) {
  format(sprintf("%.*f", d, k), width=w, justify="right", scientific=FALSE)
}


.fmti <- function(k, w=0) {
  format(sprintf("%i", k), width=w, justify="right")
}


.fmtc <- function(k, w=0, j="right") {
  format(sprintf("%s", k), width=w, justify=j)
}


.fmtNS <- function(k) {
  format(k, scientific=FALSE )
}


.xstatus <- function(var.name, dname, quiet=FALSE) {

  # see if analysis from data is based on a formula
  is.frml <- ifelse (grepl("~", var.name), TRUE, FALSE)

  # see if analysis is from descriptive stats or from data
  from.data <- ifelse (var.name == "NULL", FALSE, TRUE)

  # see if the variable exists in the Global Environment
  in.global <- FALSE
  if (nchar(var.name)>0) if (exists(var.name, where=.GlobalEnv)) {
    if (!is.function(var.name)) { # a global "var" could be a function call
      in.global <- TRUE
      if (!quiet)
        cat(">>> Note: ", var.name, "exists in the workspace, outside of",
            "a data frame (table)\n")
    }
  }

  # see if "variable" is really an expression
  if (grepl("(", var.name, fixed=TRUE) ||  grepl("[", var.name, fixed=TRUE))  {
    txtA <- paste("A referenced variable in a lessR function can only be\n",
            "a variable name.\n\n", sep="")
    txtB <- "For example, this does not work:\n  > Histogram(rnorm(50))\n\n"
    txtC <- "Instead do this:\n  > Y <- rnorm(50)\n  > Histogram(Y)"
    cat("\n"); stop(call.=FALSE, "\n","------\n",
        txtA, txtB, txtC, "\n")
  }

  if (!in.global && from.data) .nodf(dname)

  return(list(ifr=is.frml, fd=from.data, ig=in.global))
}


.nodf <- function(dname) {

  # see if the data frame exists (mydata default), if x from data, not in Global Env
  if (!exists(dname, where=.GlobalEnv)) {
    if (dname == "mydata")
      txtA <- ", the default data table name, " else txtA <- " "
    txtB1 <- "So either create the data table with the Read function, or\n"
    txtB2 <- "  specify the actual data table with the parameter: data\n"
    txtB <- paste(txtB1, txtB2, sep="")
    cat("\n"); stop(call.=FALSE, "\n","------\n",
        "Data frame (table) ", dname, txtA, "does not exist\n\n", txtB, "\n")
  }

}


.xcheck <- function(var.name, dname, data) {

  if ( (!grepl(":", var.name) && !grepl(",", var.name)) ) { # x not var list

    # see if variable exists in the data frame
    if (!exists(var.name, where=data)) {
      if (dname == "mydata") {
        txt1 <- ", the default name \n\n"
        txt2 <- "Either make sure to use the correct variable name, or\n"
        txt3 <- "specify the actual data frame with the parameter: data\n"
        txt <- paste(txt1, txt2, txt3, sep="")
      }
      else
        txt <- "\n"
      cat("\n"); stop(call.=FALSE, "\n","------\n",
          "Variable ", var.name, " does not exist either by itself ",
          "(in the user's workspace),\n",
          "or in the data frame with the name of ", dname, txt, "\n",
          "To view the existing variable names enter: > names(", dname, ")\n\n")
    }
  }
}


# see if cor matrix exists as stand-alone or embedded in list structure
.cor.exists <- function(cor.nm) {

  if (!grepl("$cors", cor.nm, fixed=TRUE))  # no $cors in name
    is.there <- exists(cor.nm, where=.GlobalEnv)

  else {
    nm <- sub("$cors", "", cor.nm, fixed=TRUE)  # remove $cors from name
    if (!exists(nm, where=.GlobalEnv))  # root list exists?
      is.there <- FALSE
    else
      is.there  <- exists("cors", where=eval(parse(text=nm)))  #  cors inside?
  }
  if (!is.there) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
      "No correlation matrix entered.\n\n",
      "No object called ", cor.nm, " exists.\n\n",
      "Either enter the correct name, or calculate with: Correlation\n",
      "Or read the correlation matrix with: corRead\n\n", sep="")
  }

}


.getlabels <- function(xlab, ylab, main) {

  # get variable labels if they exist

  x.name <- getOption("xname")
  y.name <- getOption("yname")
  x.lbl <- NULL
  y.lbl <- NULL

  dname <- getOption("dname")  # not set for dependent option on tt
  if (!is.null(dname)) {
    if (exists(dname, where=.GlobalEnv))
      mylabels <- attr(get(dname, pos=.GlobalEnv), which="variable.labels")
    else
      mylabels <- NULL
  }
  else
    mylabels <- NULL

  if (!is.null(mylabels)) {
    x.lbl <- mylabels[which(names(mylabels) == x.name)]
    if (length(x.lbl) == 0) x.lbl <- NULL
    y.lbl <- mylabels[which(names(mylabels) == y.name)]
    if (length(y.lbl) == 0) y.lbl <- NULL
  }

  # axis and legend labels
  if (!missing(xlab)) {
    if (!is.null(xlab)) x.lab <- xlab
    else if (is.null(x.lbl)) x.lab <- x.name else x.lab <- x.lbl
    if (length(x.lab) == 1) if (nchar(x.lab) > 45)  # power.ttest: len > 1
      x.lab <- paste(substr(x.lab,1,45), "...")
  }
  else x.lab <- NULL

  if (!missing(ylab)) {
    if (!is.null(ylab)) y.lab <- ylab
    else if (is.null(y.lbl)) y.lab <- y.name else y.lab <- y.lbl
    if (nchar(y.lab) > 50)
      y.lab <- paste(substr(y.lab,1,50), "...")
  }
  else y.lab <- NULL

  if (!missing(main)) {
    if (!is.null(main)) main.lab <- main else main.lab <- ""
  }
  else main.lab <- NULL

  return(list(xn=x.name, xl=x.lbl, xb=x.lab,
              yn=y.name, yl=y.lbl, yb=y.lab, mb=main.lab))
}





.ncat <- function(analysis, x.name, nu, n.cat) {

      cat("\n>>>", x.name,  "is numeric,",
          "but only has", nu, "<= n.cat =", n.cat, "levels,",
          "so treat as categorical.\n\n",
          "   If categorical, can make this variable a",
          "factor with R function: factor\n\n",
          "   If numerical, to obtain the", tolower(analysis),
          "decrease  n.cat ",
          "to specify\n",
          "   a lower number of unique values.\n")

}


# get the value for a specified function argument
.get.arg <- function(argm, fc) {

  loc <- regexec(argm, fc)
  strt1 <- loc[[1]]  # beginning of argument
  if (strt1 > 0) {
    j <- strt1
    while(substr(fc, start=j, stop=j) != "\"") j <- j + 1
    strt <- j
    j <- j + 1  # first " after ,
    while(substr(fc, start=j, stop=j) != "\"") j <- j + 1
    stp <- j  # second " after ,
    value <- substr(fc, start=strt, stop=stp)
  }
  else
    value <- ""

  return(value)
}


# remove argument and character value from a function call
.rm.arg <-  function(argm, fc) {

  loc <- regexec(argm, fc)[[1]]  # beginning of argument

  if (loc > 0) {

    first.arg <- ifelse (substr(fc, loc-1, loc-1) == "(", TRUE, FALSE)

    j <- loc
    if (!first.arg)  # is not first argument, start at preceding comma
      while(substr(fc, start=j, stop=j) != ",") if (j > 0) j <- j - 1
    strt <- j  #  closing parentheses or comma before argument

    while(substr(fc, start=j, stop=j) != "\"") if (j < 1000) j <- j + 1
    j <- j + 1  # first " after ,
    while(substr(fc, start=j, stop=j) != "\"") if (j < 1000) j <- j + 1
    stp <- j  # second " after ,

    if (first.arg) stp <- stp + 2  # remove trailing comma and space

    remv <- substr(fc, start=strt, stop=stp)
    fc.new <- sub(remv, "", fc, fixed=TRUE)

  }

  return(fc.new)
}


# remove argument and Non-String value from a function call
.rm.arg.ns <-  function(argm, fc) {

  loc <- regexec(argm, fc)[[1]]  # beginning of argument

  if (loc > 0) {

    first.arg <- ifelse (substr(fc, loc-1, loc-1) == "(", TRUE, FALSE)

    j <- loc
    if (!first.arg)  # is not first argument, start at preceding comma
      while(substr(fc, start=j, stop=j) != ",") if (j > 0) j <- j - 1
    strt <- j  #  closing parentheses or comma before argument

    dlm <- c(",", ")")

    j <- j + 1
    while(!(substr(fc, start=j, stop=j) %in% dlm))
      if (j < 1000) j <- j + 1

    stp <- j  # got a "," or a ")"
    stp <- stp - 1  # retain the "," or ")"

    if (first.arg) stp <- stp + 2  # remove trailing comma and space

    remv <- substr(fc, start=strt, stop=stp)
    fc.new <- sub(remv, "", fc, fixed=TRUE)

  return(fc.new)
  }

}


#' @title Progress Bar
#' @param style An integer for style.
#' @param active A logical value.
#' @export
#'
.progress <- function(style = 3, active = TRUE, ...) {
  ntasks <- 0
  txt <- NULL
  max <- 0

  if (active) {
    list(
      init = function(x) {
        txt <<- utils::txtProgressBar(max = x, style = style, ...)
        utils::setTxtProgressBar(txt, 0)
        max <<- x
      },
      step = function() {
        ntasks <<- ntasks + 1
        utils::setTxtProgressBar(txt, ntasks)
        if (ntasks == max) cat("\n")
      },
      term = function() close(txt)
    )
  } else {
    list(
      init = function(x) NULL,
      step = function() NULL,
      term = function() NULL
    )
  }
} # end progress bar







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
#' dotfy(x)
#'
#' @export
`dotfy` <- function(x){
  round(as.numeric(gsub(",", ".", gsub("\\.", "", x))),2)
}
NULL



`.freq` <-
  function(x, weighs = NULL, breaks = graphics::hist(x, plot = FALSE)$breaks, digits=3, include.lowest = TRUE, ord = c("level", "desc", "asc", "name"),
           useNA = c("no", "ifany", "always"),...){

    # check if x is a vector (do not use is.vector())
    if(!(is.atomic(x) || is.list(x))) stop("'x' must be a vector")

    if(inherits(x, "table")){
      tab <- x

    } else {

      if(is.numeric(x)){
        x <- base::cut(x, breaks = breaks, include.lowest = include.lowest, ordered_result = TRUE,...)
      }

      tab <- base::table(x, useNA = useNA)
    }

    # how should the table be sorted, by name, level or frq? (NULL means "desc")
    switch(match.arg(ord, c("level", "desc", "asc", "name")),
           level  = {  }
           , name   = { tab <- tab[rownames(tab)] }
           , asc    = { tab <- sort(tab) }
           , desc   = { tab <- -sort(-tab) }
    )

    ptab <- base::prop.table(tab)
    names(tab)[is.na(names(tab))] <- "<NA>"

    z <- data.frame(class = names(tab),
                    freq = as.vector(tab[]), perc = round(as.vector(ptab[]),digits),
                    cumfreq = cumsum(tab[]), cumperc = round(cumsum(ptab[]),digits))

    rownames(z) <- NULL # enumerate from 1:nrow(z)
    class(z) <- c("freq", "data.frame")
    return(z)
  }
NULL







# from rstudio/dygraphs https://github.com/rstudio/dygraphs
asISO8601Time <- function(x) {
  if (!inherits(x, "POSIXct"))
    x <- try({as.POSIXct(x, tz = "GMT")})
  # if posix conversion worked
  if (inherits(x, "POSIXct")) {
    format(x, format="%04Y-%m-%dT%H:%M:%SZ", tz='GMT')
  } else {
    # if not just return x and keep pluggin away
    x
  }
}



tableau_colors <- function(palette="tableau20") {

  x <- list(
    tableau20 =
      c("#1F77B4", "#AEC7E8", "#FF7F0E", "#FFBB78", "#2CA02C", "#98DF8A",
        "#D62728", "#FF9896", "#9467BD", "#C5B0D5", "#8C564B", "#C49C94",
        "#E377C2", "#F7B6D2", "#7F7F7F", "#C7C7C7", "#BCBD22", "#DBDB8D",
        "#17BECF", "#9EDAE5"),
    ## Tablau10 are odd, Tableau10-light are even
    tableau10medium =
      c("#729ECE", "#FF9E4A", "#67BF5C", "#ED665D", "#AD8BC9", "#A8786E",
        "#ED97CA", "#A2A2A2", "#CDCC5D", "#6DCCDA"),
    gray5 =
      c("#60636A", "#A5ACAF", "#414451", "#8F8782", "#CFCFCF"),
    colorblind10 =
      c("#006BA4", "#FF800E", "#ABABAB", "#595959", "#5F9ED1", "#C85200",
        "#898989", "#A2C8EC", "#FFBC79", "#CFCFCF"),
    trafficlight =
      c("#B10318", "#DBA13A", "#309343", "#D82526", "#FFC156", "#69B764",
        "#F26C64", "#FFDD71", "#9FCD99"),
    purplegray12 =
      c("#7B66D2", "#A699E8", "#DC5FBD", "#FFC0DA", "#5F5A41", "#B4B19B",
        "#995688", "#D898BA", "#AB6AD5", "#D098EE", "#8B7C6E", "#DBD4C5"),
    ## purplegray6 is oddd
    bluered12 =
      c("#2C69B0", "#B5C8E2", "#F02720", "#FFB6B0", "#AC613C", "#E9C39B",
        "#6BA3D6", "#B5DFFD", "#AC8763", "#DDC9B4", "#BD0A36", "#F4737A"),
    ## bluered6 is odd
    greenorange12 =
      c("#32A251", "#ACD98D", "#FF7F0F", "#FFB977", "#3CB7CC", "#98D9E4",
        "#B85A0D", "#FFD94A", "#39737C", "#86B4A9", "#82853B", "#CCC94D"),
    ## greenorange6 is odd
    cyclic =
      c("#1F83B4", "#1696AC", "#18A188", "#29A03C", "#54A338", "#82A93F",
        "#ADB828", "#D8BD35", "#FFBD4C", "#FFB022", "#FF9C0E", "#FF810E",
        "#E75727", "#D23E4E", "#C94D8C", "#C04AA7", "#B446B3", "#9658B1",
        "#8061B4", "#6F63BB")
  )

  return(x[[palette]])
}
