if (getRversion() >= "2.15.1")
  globalVariables(c(".data", ".dp.main"))


.onAttach <- function(...) {
  # Send message
  msg <- paste("\n\n")
  msg <- paste(msg,"                        000--------001\n")
  msg <- paste(msg,"                          |\\       |\\\n")
  msg <- paste(msg,"                          | \\      | \\\n")
  msg <- paste(msg,"                          |100--------101\n")
  msg <- paste(msg,"                        010--|- - -011|\n")
  msg <- paste(msg,"                           \\ |      \\ |\n")
  msg <- paste(msg,"                            \\|       \\|\n")
  msg <- paste(msg,"                           110--------111\n")
  suppressMessages(packageStartupMessage(msg))

  options(colors="dodgerblue")
  options(trans.fill.bar=0.25)
  options(trans.fill.pt=0.66)
  options(col.fill.bar = "#1874CCBF")  # .maketrans of dodgerblue3"
  options(col.fill.pt = "#1874CCBF")  # .maketrans of "dodgerblue3"
  options(col.stroke.bar="steelblue4")
  options(col.stroke.pt="steelblue4")
  options(col.bg="#EDEFF1")
  options(col.grid="snow3")
  options(col.ghost=FALSE)
  options(col.heat="dodgerblue4")

  options(n.cat=0)
  options(quiet=FALSE)
  options(brief=FALSE)

  options(explain=TRUE)
  options(interpret=TRUE)
  options(results=TRUE)
  options(document=TRUE)
  options(code=TRUE)

  options(show.signif.stars=FALSE)
  options(scipen=30)
}
NULL


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


.plotList <- function(plot.i, plot.title) {
  mxttl <- 0
  for (i in 1:plot.i)
    if (nchar(plot.title[i]) > mxttl) mxttl <- nchar(plot.title[i])
  mxttl <- mxttl + 8
  cat("\n")
  .dash(mxttl, newline=FALSE)
  for (i in 1:plot.i) {
    cat("\n", "Plot ", i,": ", plot.title[i], sep="")
  }
  cat("\n")
  .dash(mxttl, newline=FALSE)
  cat("\n\n")

}


.plotList2 <- function(plot.i, plot.title) {
  tx <- character(length = 0)

  mxttl <- 0
  for (i in 1:plot.i)
    if (nchar(plot.title[i]) > mxttl) mxttl <- nchar(plot.title[i])
  mxttl <- mxttl + 8

  tx[length(tx)+1] <- .dash2(mxttl)
  for (i in 1:plot.i)
    tx[length(tx)+1] <- paste("Plot ", i,": ", plot.title[i], sep="")
  tx[length(tx)+1] <- .dash2(mxttl)

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


.varlist <- function(n.pred, i, var.name, pred.lbl, n.obs, n.keep, lvls=NULL) {

  if (i == 1)
    txt <- "Response Variable:  "
  else
    if (n.pred > 1) txt <- paste(pred.lbl, " ",
      toString(i-1), ": ", sep="")
    else txt <- "Predictor Variable: "
  cat(txt, var.name)

  dname <- getOption("dname")
  if (exists(dname, where=.GlobalEnv))
    mylabels <- attr(get(dname, pos=.GlobalEnv), which="variable.labels")
  else
    mylabels <- NULL

  if (!is.null(mylabels)) {
    lbl <- mylabels[which(names(mylabels) == var.name)]
    if (!is.null(lbl)) cat(", ", as.character(lbl))
  }

  if (!is.null(lvls)) if (i > 1) cat("\n  Levels:", lvls)
  cat("\n")

  if (i == n.pred+1) {
    cat("\n")
    cat("Number of cases (rows) of data: ", n.obs, "\n")
    cat("Number of cases retained for analysis: ", n.keep, "\n")
  }
}


.varlist2 <- function(n.pred, i, var.name, pred.lbl, n.obs, lvls=NULL) {
  tx <- character(length = 0)

  if (i == 1)
    txt <- "Response Variable:  "
  else
    if (n.pred > 1) txt <- paste(pred.lbl, " ",
      toString(i-1), ": ", sep="")
    else txt <- "Predictor Variable: "
  tx[length(tx)+1] <- paste(txt, var.name, sep="")

  dname <- getOption("dname")
  if (exists(dname, where=.GlobalEnv))
    mylabels <- attr(get(dname, pos=.GlobalEnv), which="variable.labels")
  else
    mylabels <- NULL
  if (exists(dname, where=.GlobalEnv))
    myunits <- attr(get(dname, pos=.GlobalEnv), which="variable.units")
  else
    myunits <- NULL

  if (!is.null(mylabels)) {
    lbl <- mylabels[which(names(mylabels) == var.name)]
    unt <- myunits[which(names(myunits) == var.name)]
    if (!is.null(unt)) if (nzchar(unt))
      lbl <- paste(lbl, " (", unt, ")", sep="")
    else
      lbl <- lbl
    if (!is.null(lbl))
      tx[length(tx)] <- paste(tx[length(tx)], ", ", as.character(lbl), sep="")
  }

  if (!is.null(lvls)) if (i > 1) tx[length(tx)+1] <- paste("\n  Levels:", lvls)

  return(tx)
}


.title <- function(x.name, y.name, x.lbl, y.lbl, isnullby) {

  txt1 <- x.name
  if (!is.null(x.lbl)) txt1 <- paste(txt1, ", ", x.lbl, sep="")

  if (isnullby) txt1 <- paste("---", txt1, "---")
  else {
    txt2 <- paste(y.name, sep="")
    if (!is.null(y.lbl)) txt2 <- paste(txt2, ", ", y.lbl, sep="")
  }

  cat("\n")
  cat(txt1, "\n")
  if (!isnullby) {
    cat("  - by levels of - \n")
    cat(txt2, "\n")
    ndash <- max(nchar(txt1),nchar(txt2))
    .dash(ndash)
  }
  cat("\n")

}

.title2 <- function(x.name, y.name, x.lbl, y.lbl, isnullby, new.ln=TRUE) {

  txt1 <- x.name
  if (!is.null(x.lbl)) txt1 <- paste(txt1, ", ", x.lbl, sep="")

  if (isnullby) {
    txt1 <- paste("---", txt1, "---")
    if (new.ln) txt1 <- paste(txt1, "\n", sep="")
  }
  else {
    txt2 <- paste(y.name, sep="")
    if (!is.null(y.lbl)) txt2 <- paste(txt2, ", ", y.lbl, sep="")
  }

  tx <- character(length = 0)

  tx[length(tx)+1] <- txt1
  if (!isnullby) {
    if (is.null(y.lbl))
      tx[length(tx)+1] <- "  - by levels of - "
    else
      tx[length(tx)+1] <- "\n  - by levels of - \n"
    tx[length(tx)+1] <- txt2
    if (is.null(y.lbl))
      tx[length(tx)+1] <- .dash2(max(nchar(txt1),nchar(txt2)))
  }

  return(tx)

}


.showfile <- function(fname, txt) {
  if (getwd() == "/")
    workdir <- "top level (root) of your file system"
  else
    workdir <- getwd()
  cat("\n")
  cat("The", txt, "written at the current working directory.\n")
  cat("       ", fname, " in:  ", workdir, "\n")
  cat("\n")
}


.showfile2 <- function(fname, txt) {
  if (getwd() == "/")
    workdir <- "top level (root) of your file system"
  else
    workdir <- getwd()

  tx <- character(length = 0)

  tx[length(tx)+1] <- paste("The", txt, "written at the current working directory.")
  tx[length(tx)+1] <- paste("       ", fname, " in:  ", workdir)

  return(tx)

}


.pdfname <- function(analysis, x.name, go.pdf, pdf.nm, pdf.file) {
  if (go.pdf) {
    if (pdf.nm)
      if (!grepl(".pdf", pdf.file))
        pdf.fnm <- paste(pdf.file, ".pdf", sep="")
      else
        pdf.fnm <- pdf.file
    else
      pdf.fnm <- paste(analysis, "_", x.name, ".pdf", sep="")
  }
  else
    pdf.fnm <- NULL

  return(pdf.fnm)
}


# see if manage graphics or just sequentially plot
.graphman <- function() {

  in.RStudio <- ifelse (options("device") != "RStudioGD", FALSE, TRUE)

  in.knitr <- ifelse (is.null(options()$knitr.in.progress), FALSE, TRUE)

  manage.gr <- ifelse (!in.RStudio  &&  !in.knitr, TRUE, FALSE)

  return(manage.gr)
}


# manages the graphics system (not in RStudio or knitr)
.graphwin <- function(wnew=1, d.w=NULL, d.h=NULL) {

  dl <- dev.list()
  dl2 <- dl[which(dl==2)]  # device #2
  dl.more <- dl[which(dl>2)]  # devices larger than #2

  # remove all open windows past device 2
  if (length(dl.more) > 0) {
    min.dd <- dl.more[which(dl.more==min(dl.more))]
    max.dd <- dl.more[which(dl.more==max(dl.more))]
    for (i in min.dd:max.dd) dev.off(which=i)
  }

  off.two <- ifelse (length(dl2) == 0, TRUE, FALSE)

  # open graphics windows
  # if not already present, generate a null window for #2 and then remove
  if (off.two) wnew <- wnew + 1
  for (i in 1:wnew)
    if (is.null(d.w) && is.null(d.h))
      dev.new()
    else
      dev.new(width=d.w, height=d.h)
  if (off.two) dev.off(which=2)

}


.opendev <- function(pdf.fnm, pdf.width, pdf.height) {

  if (is.null(pdf.fnm)) {
    if (options("device") != "RStudioGD"  &&  is.null(options()$knitr.in.progress)) {
      .graphwin(1)
      orig.params <- par(no.readonly=TRUE)
      on.exit(par(orig.params))
    }
  }
  else
    pdf(file=pdf.fnm, width=pdf.width, height=pdf.height)

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


.corcolors <- function(R, NItems, main, bm=3, rm=3, diag=NULL,
                       pdf.file, pdf.width, pdf.height) {

    if (!is.null(diag)) {
      for (i in 1:NItems) R[i,i] <- diag
      cat("\nNote: To provide more color separation for off-diagonal\n",
          "      elements, the diagonal elements of the matrix for\n",
          "      computing the heat map are set to 0.\n", sep="")
    }

    max.color <- getOption("col.heat")
    hmcols <- colorRampPalette(c("white",max.color))(256)

    .opendev(pdf.file, pdf.width, pdf.height)  # set up graphics

    heatmap(R[1:NItems,1:NItems], Rowv=NA, Colv="Rowv", symm=TRUE,
      col=hmcols, margins=c(bm,rm), main=main)

    if (!is.null(pdf.file)) {  # terminate pdf graphics
      dev.off()
      .showfile(pdf.file, "plot")
    }
}


.maketrans <- function(col.name, trans.level) {
  r.tr <- col2rgb(col.name)[1]
  g.tr <- col2rgb(col.name)[2]
  b.tr <- col2rgb(col.name)[3]

  #trans.level <- (1-trans.level) * 256
  col.trans <- rgb(r.tr, g.tr, b.tr, alpha=trans.level, maxColorValue=256)

  return(col.trans)
}


.to256 <- function(trans.level)
   trn <- (1-getOption(trans.level))*256


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




.outliers <- function(x) {

  outliers <- boxplot.stats(x)$out
  if (length(outliers>0) && unique(na.omit(x)>3)) {
    cat("\nNumber of outliers:", length(outliers), "\n")

    lo.whisker <- boxplot.stats(x)$stats[1]
    lo.out <- outliers[outliers < lo.whisker]
    lo.out <- sort(lo.out, decreasing=FALSE)
    lo.len <- length(lo.out)
    cat("Small: ")
    if (lo.len > 0) {
      if (lo.len <= 25)
        for (i in 1:lo.len) cat(format(lo.out[i], scientific=FALSE), " ")
      else {
        for (i in 1:16) cat(format(lo.out[i], scientific=FALSE), " ")
        cat ("...  ")
        for (i in (lo.len-5):lo.len) cat(format(lo.out[i], scientific=FALSE), " ")
      }
    }
    else
      cat("none")
    cat("\n")

    hi.whisker <- boxplot.stats(x)$stats[5]
    hi.out <- outliers[outliers > hi.whisker]
    hi.out <- sort(hi.out, decreasing=FALSE)
    hi.len <- length(hi.out)
    cat("Large: ")
    if (hi.len > 0) {
      if (hi.len <= 25)
        for (i in 1:hi.len) cat(format(hi.out[i], scientific=FALSE), " ")
      else {
        for (i in 1:16) cat(format(hi.out[i], scientific=FALSE), " ")
        cat ("...  ")
        for (i in (hi.len-5):hi.len) cat(format(hi.out[i], scientific=FALSE), " ")
      }
    }
    else
      cat(" none\n")

  }

  cat("\n")
}


.outliers2 <- function(x) {

  tx <- character(length = 0)

  outliers <- boxplot.stats(x)$out

  if (length(outliers>0) && length(unique(na.omit(x)>3))) {
    tx[length(tx)+1] <- paste("Number of outliers:", length(outliers))

    lo.whisker <- boxplot.stats(x)$stats[1]
    lo.out <- outliers[outliers < lo.whisker]
    lo.out <- sort(lo.out, decreasing=FALSE)
    lo.len <- length(lo.out)
    tx[length(tx)+1] <- "Small: "
    if (lo.len > 0) {
      if (lo.len <= 25) {
        for (i in 1:lo.len)
          tx[length(tx)] <- paste(tx[length(tx)], .fmtNS(lo.out[i]))
      }
      else {
        for (i in 1:16)
          tx[length(tx)] <- paste(tx[length(tx)], .fmtNS(lo.out[i]))
        tx[length(tx)] <- "...  "
        for (i in (lo.len-5):lo.len)
          tx[length(tx)] <- paste(tx[length(tx)], .fmtNS(lo.out[i]))
      }
    }
    else
      tx[length(tx)] <- paste(tx[length(tx)], "none")

    hi.whisker <- boxplot.stats(x)$stats[5]
    hi.out <- outliers[outliers > hi.whisker]
    hi.out <- sort(hi.out, decreasing=FALSE)
    hi.len <- length(hi.out)
    tx[length(tx)+1] <- "Large:"
    if (hi.len > 0) {
      if (hi.len <= 25) {
        for (i in 1:hi.len)
          tx[length(tx)] <- paste(tx[length(tx)], .fmtNS(hi.out[i]))
      }
      else {
        for (i in 1:16)
          tx[length(tx)] <- paste(tx[length(tx)], .fmtNS(hi.out[i]))
        tx[length(tx)] <- paste(tx[length(tx)], "...  ")
        for (i in (hi.len-5):hi.len)
          tx[length(tx)] <- paste(tx[length(tx)], .fmtNS(hi.out[i]))
      }
    }
    else
      tx[length(tx)] <- paste(tx[length(tx)], "none")

  }

  else
    tx <- ""

  return(tx)
}


.prntbl <- function(x, digits.d=2, cut=0, cc="-", cors=FALSE,
                    brk=NULL, bnd=NULL) {
  tx <- character(length = 0)

  max.ch <- ifelse (cors, 3, 0)  # max char per column, 0 is not applicable

  # width of column 1
  max.c1 <- 0
  for (i in 1:nrow(x)) {
    c1 <- nchar(rownames(x)[i])
    if (c1 > max.c1) max.c1 <- c1
  }
  max.c1 <- max.c1 + 2

  # widths of variable names
  colnm.w <- integer(length=ncol(x))
  for (i in 1:ncol(x))
    colnm.w[i] <- nchar(colnames(x)[i])

  # width of columns
  max.ln <- integer(length=ncol(x))
  for (j in 1:ncol(x)) {
    if (is.numeric(x[,j])) {
      c.val <- 0
      for (i in 1:nrow(x)) {
        i.val <- nchar(formatC(x[i,j], digits=digits.d, format="f"))
        if (i.val > c.val) c.val <- i.val
      }
    }
    else
      c.val <- 4
    if (!cors)
      max.ln[j] <- max(colnm.w[j], c.val) + 1
    else {
      max.ln[j] <- max(colnm.w[j], 4)
      if (max.ch > 0) max.ln[j] <- max.ch
      if (max.ln[j] > 4) max.ln[j] <- max.ln[j] + 1
    }
    if (max.ln[j] < 4) max.ln[j] <- 4
  }

  if (!is.null(cc)) tx[length(tx)+1] <- .dash2(sum(max.ln)+max.c1, cc=cc)

  # matrix for potentially multi-row column names
  if (max.ch > 0) {
    nr.ind.lbl <- integer(length=ncol(x))
    for (i in 1:ncol(x))
      nr.ind.lbl[i] <- ((nchar(colnames(x)[i]) + (max.ch-1)) %/% max.ch)

    nr.lbl <- max(nr.ind.lbl)  # n row of labels
    col.nm <- matrix(nrow=nr.lbl, ncol=ncol(x))
    for (i in 1:nrow(col.nm)) {
      for (j in 1:ncol(col.nm)) {
        srt <- ((i-1)*max.ch) + 1
        stp <- srt + (max.ch - 1)
        col.nm[i,j] <- substr(colnames(x)[j], srt, stp)
        #if (nchar(col.nm[i,j]) > 0)  # left adjust within column
          #while (nchar(col.nm[i,j]) <= (max.ch-1))
            #col.nm[i,j] <- paste(col.nm[i,j], " ", sep="")
      }
    }
  }
  else {
    nr.lbl <- 1
    col.nm <- matrix(nrow=1, ncol=ncol(x))
    for (j in 1:ncol(col.nm)) col.nm[1,j] <- colnames(x)[j]
  }
  # for each row, shift down value if next row is "", repeat
  if (nr.lbl > 1) {
    for (k in 2:nrow(col.nm)) {  # repeat for each row
      for (i in 2:nrow(col.nm)) {
        for (j in 1:ncol(col.nm)) {
          if (nchar(col.nm[i,j]) == 0) {
            col.nm[i,j] <- col.nm[i-1,j]
            col.nm[i-1,j] <- ""
          }
        }
      }
    }
  }

  # write col labels
  for (i in 1:nr.lbl) {  # for each row of column labels
    tx[length(tx)+1] <- format("", width=max.c1)
    for (j in 1:ncol(x)) {
      wd <- max.ln[j]
      tx[length(tx)] <- paste(tx[length(tx)], .fmtc(col.nm[i,j], w=wd), sep="")
      if (!is.null(bnd)) if (j %in% bnd)
        if (i == nr.lbl)
          tx[length(tx)] <- paste(tx[length(tx)], "|", sep="")
        else
          tx[length(tx)] <- paste(tx[length(tx)], " ", sep="")
    }
  }
  if (!is.null(bnd))
    tx[length(tx)+1] <- .dash2(sum(max.ln)+max.c1+length(bnd), cc="-")

  # factor vars to char vars
  if (is.data.frame(x)) {
    i.col <- sapply(x, is.factor)
    x[i.col] <- lapply(x[i.col], as.character)
  }

  # write values
  for (i in 1:nrow(x)) {
    if (i %in% brk) tx[length(tx)+1] <- "..."
    rwnm <- paste(" ", rownames(x)[i])
    tx[length(tx)+1] <- format(rwnm, width=max.c1, justify="right")

    for (j in 1:ncol(x)) {
      if (is.integer(x[i,j]))
        tx[length(tx)] <- paste(tx[length(tx)], .fmti(x[i,j], w=max.ln[j]), sep="")

      else if (is.numeric(x[i,j])) {
        wd <- max.ln[j]
        if (cors) {
          if (max.ln[j] > 5) wd <- max(6, max.ln[j]+1) + 1
          else wd <- max(6, max.ln[j]+1)
          cs <- .fmt(x[i,j], d=digits.d, w=wd)
          cs <- sub("0.", "", cs, fixed=TRUE)
          cs <- sub(" 1.00", "100", cs, fixed=TRUE)
        }
        else
          cs <- .fmt(x[i,j], d=digits.d, w=wd)
        if (abs(x[i,j]) < cut) cs <- paste(rep(" ", wd-2), collapse="")
        tx[length(tx)] <- paste(tx[length(tx)], cs, sep="")
        if (!is.null(bnd)) if (j %in% bnd)
          tx[length(tx)] <- paste(tx[length(tx)], "|", sep="")
      }

      else if (is.character(x[i,j]))
        tx[length(tx)] <- paste(tx[length(tx)], .fmtc(x[i,j], w=max.ln[j]) , sep="")
    }

    if (!is.null(bnd)) if (i %in% bnd)
      tx[length(tx)+1] <- .dash2(sum(max.ln)+max.c1+length(bnd), cc="-")
  }

  return(tx)

}  # end .prntbl


.ss.factor <-
function(x, by=NULL, brief=FALSE, digits.d=NULL, ...)  {


# construct a cross-tabs
.prnfreq <- function(x, type, max.ln, max.c1, n.dash, ttl) {
  tx <- character(length = 0)

  # title
  tx[length(tx)+1] <- .dash2(n.dash);
  tx[length(tx)+1] <- ttl
  tx[length(tx)+1] <- .dash2(n.dash);

  # col labels
  tx[length(tx)+1] <-  .fmtc(x.name, w=max.c1+3)
  tx[length(tx)+1] <-  format(y.name, width=max.c1, justify="left")
  w <- nchar(as.character(sum(x)))
  for (i in 1:ncol(x))
    tx[length(tx)] <- paste(tx[length(tx)], .fmtc(colnames(x)[i], w=max.ln[i]),
      sep="")

  # values
  for (i in 1:nrow(x)) {
    rwnm <- paste(" ", rownames(x)[i])
    tx[length(tx)+1] <-  format(rwnm, width=max.c1, justify="left")
    for (j in 1:ncol(x)) {
      if (type=="r") {
        tx[length(tx)] <- paste(tx[length(tx)], .fmt(x[i,j], d=3, w=max.ln[j]),
          sep="")
      }
      else if (type=="i")
        tx[length(tx)] <- paste(tx[length(tx)], .fmti(x[i,j], w=max.ln[j]),
          sep="")
    }
  }

  return(tx)
}  # end .prnfreq


  # get variable labels if exist
  gl <- .getlabels()
  x.name <- gl$xn; x.lbl <- gl$xl
  y.name <- gl$yn; y.lbl <- gl$yl

  # save ordered status before converting x to a table
  if (is.ordered(x) && is.null(by)) order.x <- TRUE else order.x <- FALSE
  if (is.ordered(by)) order.y <- TRUE else order.y <- FALSE

  # convert to table, with variable names, if needed
  if (!is.table(x) && !is.matrix(x)) {  # bc yields a table or matrix
    if (!is.null(by))
      x <- table(by,x, dnn=c(y.name,x.name))
    else x <- table(x, dnn=NULL)
  }


  # no title if two vars and no labels
  txttl <- ""
  if (is.null(by) || (!is.null(x.lbl) || !is.null(y.lbl))) { #  one var or labels
    txttl <- .title2(x.name, y.name, x.lbl, y.lbl, is.null(by), new.ln=FALSE)
  }

  # print table, chi-square analysis
  # -------------------------------------
  # two variables
  if (!is.null(by) || is.matrix(x)) {
    n.dim <- 2

    xx <- addmargins(x)

    # width of column 1
    if (!is.null(y.name))
      max.c1 <- nchar(y.name)
    else
      max.c1 <- 0
    for (i in 1:nrow(xx)) {
      c1 <- nchar(rownames(xx)[i])
      if (c1 > max.c1) max.c1 <- c1
    }
    max.c1 <- max.c1 + 2
    if (max.c1 < 5) max.c1 <- 5

    # width of data columns
    max.ln <- integer(length=0)
    for (i in 1:ncol(xx)) {
      ln.nm <- nchar(colnames(xx)[i])
      ln.vl <- nchar(as.character(xx)[i])
      max.ln[i] <- max(ln.nm, ln.vl) + 1
      if (max.ln[i] < 4) max.ln[i] <- 4
    }

    # Cell frequencies
    txfrq <- .prnfreq(xx, "i", max.ln, max.c1, n.dash=32,
                      ttl="Joint and Marginal Frequencies")

    if (brief)
      return(list(n.dim=n.dim, txttl=txttl, txfrq=txfrq))


    # full analysis
    nan.flag <- FALSE

    for (i in 1:ncol(xx)) {
      if (max.ln[i] < 6) max.ln[i] <- 6
    }

    # Cell Proportions and Marginals
    xx <- round(addmargins(prop.table(x)),3)
    txprp <- .prnfreq(xx, "r", max.ln, max.c1, n.dash=30,
                      ttl="Cell Proportions and Marginals")

    # Cell Proportions within Each Column
    x.col <- prop.table(x, margin=2)
    Sum <- numeric(ncol(x.col))
    for (i in 1:ncol(x.col)) {
      Sum[i] <- sum(x.col[,i])
      if (is.nan(Sum[i])) nan.flag <- TRUE
    }
    x.col2 <- round(rbind(x.col,Sum),3)
    names(dimnames(x.col2)) <- names(dimnames(x.col))

    txcol <- .prnfreq(x.col2, "r", max.ln, max.c1, n.dash=35,
                      ttl="Cell Proportions within Each Column")

    # Cell Proportions within Each Row
    x.row <- prop.table(x, margin=1)
    Sum <- numeric(nrow(x.row))
    for (i in 1:nrow(x.row)) {
      Sum[i] <- sum(x.row[i,])
      if (is.nan(Sum[i])) nan.flag <- TRUE
    }
    x.row2 <- round(cbind(x.row,Sum),3)
    names(dimnames(x.row2)) <- names(dimnames(x.row))

    txrow <- .prnfreq(x.row2, "r", max.ln, max.c1, n.dash=32,
                      ttl="Cell Proportions within Each Row")

    if (nan.flag)
      cat("\nNote: NaN results from all values missing for that cell or margin.\n",
                 "     so any division to compute a proportion is undefined.\n")

    return(list(n.dim=n.dim, txttl=txttl, txfrq=txfrq, txprp=txprp,
                txcol=txcol, txrow=txrow))  # back to ss or ss data frame

    # end full analysis

  }  # end two variable


  else {  # one variable
    n.dim <- 1

    if (length(names(x)) == sum(x)) {
      cat("\nAll values are unique.  Perhaps a row ID instead of a variable.\n",
          "If so, use  row.names  option when reading. See help(read.table).\n\n", sep="")
      if (sum(x) < 100) print(names(x))
      else cat("\nOnly the first 100 values listed.  To see all, use\n",
               "the  values  function.\n\n")
    }
    else {

      max.ln <- integer(length=0)
      for (i in 1:length(x)) {
        ln.nm <- nchar(names(x[i]))
        ln.vl <- nchar(as.character(x[i]))
        max.ln[i] <- max(ln.nm, ln.vl) + 1
        if (max.ln[i] < 6) max.ln[i] <- 6
      }

      tx <- character(length = 0)

      tx[length(tx)+1] <-  format("", width=13)
      w <- nchar(as.character(sum(x)))
      for (i in 1:length(x))
        tx[length(tx)] <- paste(tx[length(tx)], .fmtc(names(x[i]), w=max.ln[i]))
      tx[length(tx)] <- paste(tx[length(tx)], .fmtc("Total", w=w+6))

      tx[length(tx)+1] <- "Frequencies: "
      for (i in 1:length(x))
        tx[length(tx)] <- paste(tx[length(tx)], .fmti(x[i], w=max.ln[i]))
      tx[length(tx)] <- paste(tx[length(tx)], .fmti(sum(x), w=w+6))

      tx[length(tx)+1] <- "Proportions: "
      sum.x <- sum(x)
      xp <- numeric(length=0)
      xp <- x/sum.x
      for (i in 1:length(x))
        tx[length(tx)] <- paste(tx[length(tx)], .fmt(xp[i], 3, max.ln[i]))
      tx[length(tx)] <- paste(tx[length(tx)], .fmtc("1.000", w=w+6))

      return(list(n.dim=n.dim, title=txttl, tx=tx, frq=x, prp=xp))  # back to SummaryStats
    }
  }  # one variable

} # end .ss.factor



.bc.main <-
function(x, by=NULL,
         col.fill, col.stroke, col.bg, col.grid, random.col, colors,
         horiz, over.grid, addtop, gap, prop, xlab, ylab, main,
         cex.axis, col.axis,  beside, col.low, col.hi, count.levels,
         legend.title, legend.loc, legend.labels, legend.horiz, quiet, ...) {

 if (!is.null(by) && prop) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
    "Analysis of proportions not valid for two variables.\n\n")
  }

  if (horiz && addtop!=1) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
    "addtop  only works for a vertical bar plot.\n\n")
  }

  if ( (is.table(x) || is.matrix(x)) && is.null(legend.title) ) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
    "Need to specify a value for:   legend.title\n\n")
  }

  # get variable labels if exist plus axes labels
  if (is.null(ylab)) if (!prop) ylab <- "Frequency" else ylab <- "Proportion"
  gl <- .getlabels(xlab, ylab, main)
  x.name <- gl$xn; x.lbl <- gl$xl; x.lab <- gl$xb
  y.name <- gl$yn; y.lbl <- gl$yl; y.lab <- gl$yb
  main.lab <- gl$mb

  # if a table, convert to a matrix
  if (is.table(x)) {
    xr.nm <- rownames(x)
    xc.nm <- colnames(x)  # as.numeric makes equivalent to matrix input
    x <- matrix(as.numeric(x), nrow=nrow(x), ncol=ncol(x))
    rownames(x) <- xr.nm
    colnames(x) <- xc.nm
  }

  # get legend title, l.lab
  if (!is.null(legend.title)) l.lab <- legend.title
  else if (!is.null(by)) if (exists("y.lbl"))
    if (length(y.lbl) == 0) l.lab <- y.name else l.lab <- y.lbl

  # title
  if (!is.null(main))
    main.lbl <- main
  else
    main.lbl <- ""

  # entered counts typically integers as entered but stored as type double
  # if names(x) or rownames(x) is null, likely data from sample and c functions
  # count.levels is getting counts directly from a data frame with counts entered
  entered.pre <- FALSE
  if (!is.matrix(x) && !is.null(names(x))) entered.pre <- TRUE
  if (is.matrix(x) && !is.null(rownames(x))) entered.pre <- TRUE
  if (!is.integer(x) && is.double(x) && entered.pre)
    entered <- TRUE else entered <- FALSE
  if (!is.null(count.levels)) {
    x <- as.numeric(x)
    names(x) <- count.levels
    entered <- TRUE
  }

  # save ordered status before converting x to a table
  if (is.ordered(x) && is.null(by)) order.x <- TRUE else order.x <- FALSE
  if (is.ordered(by)) order.y <- TRUE else order.y <- FALSE


  if (!entered) {
    if (!is.null(by)) x <- table(by,x, dnn=c(y.name, x.name))
    else {
      x <- table(x, dnn=NULL)
      if (prop) {
        x.temp <- x
        x <- x/sum(x)
      }
    }
  }

  if (is.null(by) && beside && !entered) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
    "beside=TRUE  is not valid for analysis of only one variable.\n\n")
  }


  # ----------------------------------------------------------------------------
  # colors

  # get number of colors (does not indicate col.fill multiple colors)
  if (is.null(by) && !order.x && !is.matrix(x))
    n.colors <- 1
  else
    n.colors <- nrow(x)
  if (!is.null(by) && order.y) n.colors <- nrow(x)

  if ( (colors == "rainbow"  ||  colors=="terrain"  || colors=="heat") ) {
    n.colors <- nrow(x)
    nogo <- FALSE
    if (is.ordered(x) && is.null(by)) nogo <- TRUE
    if (is.ordered(by)) nogo <- TRUE
    if (nogo) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
      "Can only do an R color range when there is more than one color. \n\n")
    }
  }

  # color palette
  if ((order.x && is.null(by)) || order.y) {  # one var, ordered factor
      col.grid <- getOption("col.grid")
      col.bg <- getOption("col.bg")
    if (colors == "blue") {
      if (is.null(col.low)) col.low <- "slategray2"
      if (is.null(col.hi)) col.hi <- "slategray4"
    }
    else if (colors == "gray") {
      if (is.null(col.low)) col.low <- "gray70"
      if (is.null(col.hi)) col.hi <- "gray25"
    }
    else if (colors == "gray.black") {
      if (is.null(col.low)) col.low <- "gray70"
      if (is.null(col.hi)) col.hi <- "gray25"
    }
    else if (colors == "green") {
      if (is.null(col.low)) col.low <- "darkseagreen1"
      if (is.null(col.hi)) col.hi <- "darkseagreen4"
    }
    else if (colors == "rose") {
      if (is.null(col.low)) col.low <- "mistyrose1"
      if (is.null(col.hi)) col.hi <- "mistyrose4"
    }
    else if (colors == "gold") {
      if (is.null(col.low)) col.low <- "goldenrod1"
      if (is.null(col.hi)) col.hi <- "goldenrod4"
    }
    else if (colors == "red") {
      if (is.null(col.low)) col.low <- "coral1"
      if (is.null(col.hi)) col.hi <- "coral4"
    }
    else if (colors == "orange.black") {
      if (is.null(col.low)) col.low <- rgb(255,173,91, maxColorValue=256)
      if (is.null(col.hi)) col.hi <- rgb(169,66,2, maxColorValue=256)
    }
    else if (colors == "sienna") {
      if (is.null(col.low)) col.low <- "sienna1"
      if (is.null(col.hi)) col.hi <- "sienna4"
    }
    else if (colors == "dodgerblue") {
      if (is.null(col.low)) col.low <- "dodgerblue1"
      if (is.null(col.hi)) col.hi <- "dodgerblue4"
    }
    else if (colors == "purple") {
      if (is.null(col.low)) col.low <- "purple1"
      if (is.null(col.hi)) col.hi <- "purple4"
    }
    color.palette <- colorRampPalette(c(col.low, col.hi))
    clr <- color.palette(n.colors)
  }

  else if (colors == "gray") {
    color.palette <- colorRampPalette(c("gray30","gray80"))
    clr <- color.palette(nrow(x))
    if (col.grid == "gray86") col.grid <- getOption("col.grid")
    if (col.bg == "ghostwhite") col.bg <- getOption("col.bg")
  }
  else if ((colors=="blue" || colors=="rose" || colors=="green"
          || colors=="gold" || colors=="red" || colors=="orange"
          || colors=="sienna" || colors=="dodgerblue" || colors=="purple"
          || colors=="white"
          || colors=="orange.black" || colors=="gray.black")
          && (is.null(by) && !is.matrix(x))) {
      if (n.colors > 1) {
        color.palette <- colorRampPalette(getOption("col.fill.bar"))
        clr <- color.palette(nrow(x))
      }
      col.grid <- getOption("col.grid")
      col.bg <- getOption("col.bg")
    }
    else if (colors == "rainbow") clr <- rainbow(n.colors)
    else if (colors == "terrain") clr <- terrain.colors(n.colors)
    else if (colors == "heat") clr <- heat.colors(n.colors)
    else  {  # mono color range does not make sense here
      clr <- c("slategray", "peachpuff2", "darksalmon", "darkseagreen1",
              "lightgoldenrod3", "mistyrose", "azure3", "thistle4")
      cat("\n>>> Note: For two variables, the color theme only applies to\n",
          "        the levels of an ordered factor, except for the \"gray\"\n",
          "        color theme. However, other choices are available for\n",
          "        the colors option:  \"rainbow\", \"terrain\" and \"heat\". \n\n",
          sep="")
    }

    if (random.col) clr <- clr[sample(length(clr))]

  if (!is.null(col.fill)) {
    if (n.colors > 1) {
      for (i in 1:(min(length(col.fill),length(clr)))) clr[i] <- col.fill[i]
      n.colors <- min(length(col.fill),length(clr))
    }
    else {
      col <- col.fill
    }
  }

  if (n.colors > 1) {
    palette(clr)
    col <- 1:n.colors
  }
  else {
    if (is.null(col.fill)) col <- getOption("col.fill.bar")
  }


  # ----------------------------------------------------------------------------
  # preliminaries

  if (is.matrix(x) && !beside) max.y <- max(colSums(x)) else max.y <- max(x)
  if (prop) addtop <- .01
  max.y <- max.y + addtop

  if (is.null(legend.labels)) legend.labels <- row.names(x)
# if (!is.null(legend.labels)) if (is.null(legend.loc)) legend.loc <- "topleft"
  if (beside) legend.horiz <- FALSE
  if ((!is.null(by) || is.matrix(x)) && !beside) {
    legend.horiz <- TRUE
#   if (is.null(legend.loc)) legend.loc <- "top"
    max.y <- max.y + .18*max.y
  }

  if (is.null(gap)) if (is.matrix(x) && beside) gap <- c(0.1,1) else gap <- 0.2

  # get max label size
  the.names <- integer(length=0)
  if (length(dim(x)) == 0)
    the.names <- names(x)
  else
   if (is.null(by)) the.names <- rownames(x) else the.names <- colnames(x)
  max.nm <- 0
  for (i in (1:length(the.names))) {
    li <- nchar(the.names[i])
    if (li > max.nm) max.nm <- li
  }

  # extend the left margin to accommodate horizontal labels
  extend <- FALSE
  if (horiz && max.nm>5) {
    add.left <- max.nm/2.0
    if (y.lab != "") add.left <- add.left + 1.5
    extend <- TRUE
  }


  # ----------------------------------------------------------------------------
  # set up plot area, color background, grid lines

  if (extend) par(mar=c(5, add.left, 4, 2) + 0.1)

  if (legend.loc == "right.margin"  &&  (!is.null(by) || is.matrix(x)))
    par(oma=c(0,0,0,3))

  if(is.null(count.levels)) if (horiz) {
    temp <- x.lab; x.lab <- y.lab; y.lab <- temp
  }

  if (class(x) == "numeric"  &&  entered) x <- as.table(x)
  rescale <- 0
  if (is.null(by)) if (nrow(x) <=4) rescale <- nrow(x)
  if (!is.null(by) && !beside) if (ncol(x) <=4) rescale <- ncol(x)
  if (class(x) == "matrix" && entered) rescale <- 0  # turned off for now

  # set rescale to control bar width for small number of bars
  if (rescale == 0) {
    if (!horiz)
      barplot(x, col="transparent", ylim=c(0,max.y), axisnames=FALSE,
        beside=beside, space=gap, axes=FALSE, ...)
    else
      barplot(x, col="transparent", horiz=TRUE, axisnames=FALSE,
        beside=beside, space=gap, axes=FALSE, ...)
  }
  else {
    if (rescale == 4) width.bars <- .17
    if (rescale == 3) width.bars <- .22
    if (rescale == 2) width.bars <- .28
    gap <- 0.246 + 0.687*width.bars
    if (!horiz)
      barplot(x, col="transparent", ylim=c(0,max.y), axisnames=FALSE,
        beside=beside, space=gap, width=width.bars, xlim=c(0,1), axes=FALSE, ...)
    else
      barplot(x, col="transparent", horiz=TRUE, axisnames=FALSE,
        beside=beside, space=gap, width=width.bars, ylim=c(0,1), axes=FALSE, ...)
  }

  if (extend) {
    mtext(y.lab, side=2, line=add.left-1)
    y.lab <- ""
    las.value <- 1
  }
  else las.value <- 0


  # ----------------------------------------------------------------------------
  # bar plot, grid lines

  usr <- par("usr")
  rect(usr[1], usr[3], usr[2], usr[4], col=col.bg, border="black")
  if (max.y > 1) vy <- pretty(0:max.y) else vy <- pretty(1:100*max.y)/100

  if (!over.grid) {
    if (!horiz)
      abline(h=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid, lwd=.5)
    else
      abline(v=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid, lwd=.5)
  }
  if (rescale == 0) {
#    width.bars <- .8   gap <- .6*width.bars
    barplot(x, add=TRUE, col=col, beside=beside, horiz=horiz,
          xlab=x.lab, ylab=y.lab, main=main.lbl, border=col.stroke, las=las.value,
          space=gap, cex.axis=cex.axis, cex.names=cex.axis,
          col.axis=col.axis, ...)
  }
  else
    barplot(x, add=TRUE, col=col, beside=beside, horiz=horiz,
          xlab=x.lab, ylab=y.lab, main=main.lbl, border=col.stroke, las=las.value,
          space=gap, width=width.bars, xlim=c(0,1),
          cex.axis=cex.axis, cex.names=cex.axis,
          col.axis=col.axis, ...)
  if (over.grid) {
    if (!horiz)
      abline(h=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid, lwd=.5)
    else
      abline(v=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid, lwd=.5)
  }


  # ----------------------------------------------------------------------------
  # legend for two variable plot including variable labels
  if ( (!is.null(by) || is.matrix(x)) && !is.null(legend.loc))  {

    if (col.bg != "black")
      col.txt <- "black"
    else
      col.txt <- "white"

    # default right.margin option
    if (legend.loc == "right.margin") {

      par(xpd=NA)  # allow drawing outside of plot region

      # split string into separate words
      wordlist <- as.vector(strsplit(l.lab, " "))
      n.words <- length(wordlist[[1]])

      # put elements of word list into words for ease of programming
      words <- character(length=0)
      for (i in 1:n.words) words[i] <- abbreviate(wordlist[[1]][i],12)

      # combine words into lines of max length 13
      lines <- integer(length=0)
      j <- 0
      iword1 <- 1
      while (iword1 <= n.words) {
        j <- j + 1
        lines[j] <- words[iword1]
        trial <- ""
        iword <- 0
        while (nchar(trial) < 13  &&  iword1 <= n.words) {
          trial <- paste(trial, words[iword1+iword])
          if (nchar(trial) <= 13) {
            lines[j] <- trial
            iword1 <- iword1 + 1
          }
        }
      }
      n.lines <- length(lines)

      # remove leading blank in each line
      for (i in 1:n.lines) lines[i] <- substr(lines[i], 2, nchar(lines[i]))

      # get max word in terms of user coordinates
      max.wlen <- 0
      for (i in 1:n.lines) {
        wl <- strwidth(lines[i])
        if (wl > max.wlen) {
          max.wlen <- wl
          max.word <- lines[i]
        }
      }

      # attach a line break at the end of all but the last line
      if (n.lines > 1)
        for (i in 1:(n.lines-1)) lines[i] <- paste(lines[i], "\n", sep="")

      # construct the legend title as a single character string
      l.lab2 <- ""
      for (i in 1:n.lines) l.lab2 <- paste(l.lab2, lines[i], sep="")

      # construct vertical buffer for legend for additional legend title lines
      axis.vert <- usr[4] - usr[3]
      vbuffer <- (n.lines-1)*(0.056*axis.vert)  # usr[4] is top axis coordinate

      # legend function blind to multiple line titles,
      # so pass largest word to get the proper width of the legend
      # also get height of legend with only one title line
      legend.labels <- abbreviate(legend.labels, 6)
      ll <- legend(0,0, legend=legend.labels, title=max.word, cex=.7,
                   fill=col, plot=FALSE)

      # horizontal placement
      size <- (par("cxy")/par("cin"))  # 1 inch in user coordinates
      epsilon <- (size[1] - ll$rect$w) / 2

      # legend box
      xleft <- usr[2] + epsilon   # usr[2] is the user coordinate of right axis
      xright <- xleft + ll$rect$w
      lgnd.vhalf <- (vbuffer + ll$rect$h) / 2
      axis.cntr <- axis.vert / 2  + usr[3]
      ytop <- axis.cntr + lgnd.vhalf
      ybottom <- axis.cntr - lgnd.vhalf
      rect(xleft, ybottom, xright, ytop, lwd=.5, border="gray30", col=col.bg)

      # legend not multiple title lines aware, so start at last title line
      legend(x=xleft, y=ytop-vbuffer, legend=legend.labels, title=l.lab2,
             fill=col, horiz=FALSE, cex=.7, bty="n", box.lwd=.5,
             box.col="gray30", text.col=col.txt)

    }  # right margin

    else
      legend(legend.loc, legend=legend.labels, title=l.lab, fill=col,
             horiz=legend.horiz, cex=.7, bty="n", text.col=col.txt)
  }

  # ----------------------------------------------------------------------------
  # text output
  if (prop) x  <- x.temp

  if (length(dim(x)) == 1) {  # one variable
    stats <- .ss.factor(x)

    txttl <- stats$title
    tx <- stats$tx
    class(txttl) <- "out_piece"
    class(tx) <- "out_piece"
    output <- list(out_title=txttl, out_text=tx)
    class(output) <- "out_all"
    print(output)

    if (!quiet) {
      ch <- chisq.test(x)
      pvalue <- format(sprintf("%6.4f", ch$p.value), justify="right")
      cat("\nChi-squared test of null hypothesis of equal probabilities\n")
      cat("  Chisq = ", ch$statistic, ",  df = ", ch$parameter, ",  p-value = ",
        pvalue, sep="", "\n")
      if (any(ch$expected < 5))
        cat(">>> Low cell expected frequencies,",
            "so chi-squared approximation may not be accurate", "\n")
    }
  }

  else {  # two variables
    stats <- .ss.factor(x, by, brief=TRUE)

    txttl <- stats$txttl
    txfrq <- stats$txfrq
    class(txttl) <- "out_piece"
    class(txfrq) <- "out_piece"
    output <- list(out_title=txttl, out_text=txfrq)
    class(output) <- "out_all"
    print(output)


    if (!quiet) {
      cat("\n"); .dash(19); cat("Chi-square Analysis\n"); .dash(19);
      ch <- (summary(as.table(x)))
      pvalue <- format(sprintf("%6.4f", ch$p.value), justify="right")
      cat("Number of cases (observations) in analysis:", ch$n.cases, "\n")
      cat("Number of variables:", ch$n.vars, "\n")
      cat("Test of independence: ",
          "  Chisq = ", ch$statistic, ", df = ", ch$parameter, ", p-value = ",
          pvalue, sep="", "\n")
      if (!ch$approx.ok)
        cat(">>> Low cell expected frequencies,",
            "so chi-squared approximation may not be accurate", "\n")
    }
  }

  cat("\n")

  return(stats)

} # end .bc.zmain





.bx.main <-
function(x, col.fill, col.stroke, col.bg, col.grid,
         cex.axis, col.axis,
         horiz, add.points, xlab, main, digits.d, quiet, ...) {

  if (is.null(col.stroke)) col.stroke <- col.fill

# get variable label if exists
  gl <- .getlabels(xlab, main=main)
    x.name <- gl$xn;  x.lbl <- gl$xl;  x.lab <- gl$xb
  main.lab <- gl$mb
  if (horiz) y.lab <- ""
  else {
    y.lab <- x.lab
    x.lab <- ""
  }

  # set up plot area
  bv <- (boxplot(x, col="transparent", bg="transparent",
     horizontal=horiz, xlab=x.lab, ylab=y.lab, main=main.lab, axes=FALSE, ...))
  if (horiz)
    axis(1, cex.axis=cex.axis, col.axis=col.axis, ...)
  else
    axis(2, cex.axis=cex.axis, col.axis=col.axis, ...)

  # colored background for plotting area
  usr <- par("usr")
  rect(usr[1], usr[3], usr[2], usr[4], col=col.bg, border="black")

  # grid lines computation and print
  if (horiz) {
    vx <- pretty(c(usr[1],usr[2]))
    abline(v=seq(vx[1],vx[length(vx)],vx[2]-vx[1]), col=col.grid, lwd=.5)
  }
  else {
    vy <- pretty(c(usr[3],usr[4]))
    abline(h=seq(vy[1],vy[length(vy)],vy[2]-vy[1]), col=col.grid, lwd=.5)
  }
  # box plot
  boxplot(x, add=TRUE, col=col.fill, bg=col.stroke, pch=21,
          horizontal=horiz, axes=FALSE, border=col.stroke, ...)
  #boxplot(x, add=TRUE, col=col.fill, bg=col.stroke, pch=21,
          #horizontal=horiz, axes=FALSE, border=col.stroke,
          #whiskcol=getOption("col.fill.bar"), staplecol=getOption("col.fill.bar"),
          #medcol=getOption("col.stroke.bar"))

  # dots
  if (add.points)
      .dp.main(x, by=NULL,
         col.fill, col.stroke, col.bg, col.grid, shape.pts=NULL,
         cex.axis=.85, col.axis="gray30",
          xlab=NULL, main=NULL, cex=NULL,
         method="overplot", pt.reg=21, pt.out=19,
         col.out30="firebrick2", col.out15="firebrick4",
         quiet=TRUE, new=FALSE, vertical=!horiz, ...)

  # summarize data

    n <- sum(!is.na(x))
    n.miss <- sum(is.na(x))
    mn <- .fmt(min(x, na.rm=TRUE))
    lw <- .fmt(bv$stats[1])
    lh <- .fmt(bv$stats[2])
    md <- .fmt(bv$stats[3])
    uh <- .fmt(bv$stats[4])
    uw <- .fmt(bv$stats[5])
    mx <- .fmt(max(x, na.rm=TRUE))
    IQR <- .fmt(IQR(x, na.rm=TRUE))

  tx=""
  if (!quiet) {
    digits.d <- .max.dd(x)
    options(digits.d=digits.d)

    tx <- character(length = 0)

    main.lab <- as.character(main.lab)
    if (nchar(main.lab) > 0) main.lab <- paste(",", main.lab)
    txt <- paste("--- ", x.lab, main.lab, " ---", sep="")
    tx[length(tx)+1] <- txt
    tx[length(tx)+1] <- paste("Present:", n)
    tx[length(tx)+1] <- paste("Missing:", n.miss)
    tx[length(tx)+1] <- paste("Total  :", length(x))
    tx[length(tx)+1] <- ""
    tx[length(tx)+1] <- paste("Minimum      :", mn)
    tx[length(tx)+1] <- paste("Lower Whisker:", lw)
    tx[length(tx)+1] <- paste("Lower Hinge  :", lh)
    tx[length(tx)+1] <- paste("Median       :", md)
    tx[length(tx)+1] <- paste("Upper Hinge  :", uh)
    tx[length(tx)+1] <- paste("Upper Whisker:", uw)
    tx[length(tx)+1] <- paste("Maximum      :", mx )
    tx[length(tx)+1] <- ""
    tx[length(tx)+1] <- paste("1st Quartile :", .fmt(quantile(x, na.rm=TRUE)[2]))
    tx[length(tx)+1] <- paste("3rd Quartile :", .fmt(quantile(x, na.rm=TRUE)[4]))
    tx[length(tx)+1] <- paste("IQR          :", IQR)

  }  # end !quiet

  return(list(tx=tx, n=n, n.miss=n.miss, mn=mn, lw=lw, lh=lh, md=md, uh=uh,
         uw=uw, mx=mx, IQR=IQR))

} # end .bx.main

