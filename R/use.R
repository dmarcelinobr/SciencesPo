#' @title Load or import a data file
#'
#' @description Load or import a data file from "csv", "SPSS", "R", "Excel", "SAS" formats with optional parameters.
#'
#'
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @param file The file reference, either omitted to browse for the data file, or (except for .Rda files) a full path name or web URL, included in quotes.  A URL begins with \code{http://}.
#' @param format Format of the data in the file, which by default is a \code{csv} file, which also will recognize tab-delimited text. As an option can be an Excel \code{.xls} or \code{.xlsx} file or an SPSS \code{.sav} file, which also reads the variable labels if present, or a native R data file with a file type of \code{.rda}, or a (native R) data file.
#' @param labels File name for the file of variable labels. Either a full path name, or just the file name if in the same directory as the data file, or no reference between the quotes, which allows the user to browse for the labels file. Or, if \code{row2}, then the labels are in the second line of the data file.
#' @param widths Specifies the width of the successive columns for fixed width formatted data.
#' @param missing Missing value code, which by default is literally a missing data value in the data table.
#' @param n.mcut For the missing value analysis, list the row name and number of missing values if the number of missing exceeds or equals this cutoff.
#' @param miss.show For the missing value analysis, the number of rows, one row per observation, that has as many or missing values as \code{n.mcut}.
#' @param miss.zero For the missing value analysis, list the variable name or the row name even for values of 0. By default only variables and rows with missing data are listed.
#' @param miss.matrix For the missing value analysis, if there is any missing data, list a version of the complete data table with a 0 for a non-missing value and a 1 for a missing value.
#' @param max.lines Maximum number of lines to list of the data and labels.
#' @param sheet For Excel files, specifies the work sheet to read. The default is the first work sheet.
#' @param brief If \code{TRUE}, display only variable names table plus any variable labels.
#' @param quiet If set to \code{TRUE}, no text output. Can change the corresponding system default with \code{\link{set}} function.
#' @param fun.call Function call. Used with \code{knitr} to pass the function call when obtained from the abbreviated function call \code{rd}.
#' @param \dots Other parameter values define with the R read functions, such as the \code{read.table} function for text files, with row.names and header such as \code{sep} and \code{dec}.
#'
#' @export
`use` <-
function(file=NULL, format=c("csv", "SPSS", "R", "Excel", "SAS"),
         labels=NULL, widths=NULL, missing="", n.mcut=1,
         miss.show=30, miss.zero=FALSE, miss.matrix=FALSE,
         max.lines=30, sheet=1,
         brief=getOption("brief"), quiet=getOption("quiet"),
         fun.call=NULL, ...) {
  if (is.null(fun.call)) fun.call <- match.call()
  no.format <- ifelse (missing(format), TRUE, FALSE)
  format <- match.arg(format)
# option to browse for data file, and then display file name
  if (is.null(file)) {
    file <- file.choose()
    cat("\n")
    fncl <- paste("use(", "file = \"", file,  "\", quiet = TRUE)", sep="")
  }
  else
    fncl <- .fun.call.deparse(fun.call)
  options(read.call=fncl)  # save for knitr run
  if (no.format) {
    if (grepl(".sav$", file)) format <- "SPSS"
    if (grepl(".sas7bdat$", file)) format <- "SAS"
    if (grepl(".rda$", file)) format <- "R"
    if (grepl(".xls$", file) || grepl(".xlsx$", file)) format <- "Excel"
    if (!is.null(widths)) format <- "fwd"
  }
  if (format=="Excel"  &&  grepl("http", file, fixed=TRUE)) {
    cat("\n"); stop(call.=FALSE, "\n","------\n",
        "At this time the underlying read_excel function\n",
        "does not support reading Excel files from the web.\n",
        "Can use the  download.file  function to download to the\n",
        "local file system.\n\n",
        "  download.file(\"", file, "\", \"MYFILE.xlsx\")\n\n",
        "Replace MYFILE with the desired file name.\n",
        "Enter  getwd()  to see where the file was saved. \n\n")
  }
  # construct full path name for label file if not already
  if (!is.null(labels)) {
    if (labels == "")
      file.lbl <- file.choose()
    else {
      if (labels!="row2") {
        if (!grepl("/", labels) && !grepl("\\\\", labels)) {  # not full path
          nc <- nchar(file)
          ch <- substr(file, start=1, stop=nc)
          n.gone <- 0
          while ((ch != "/")  &&  (ch != "\\")) {
            n.gone <- n.gone + 1
            ch <- substr(file, start=nc-n.gone, stop=nc-n.gone)
          }
          file.lbl <- substr(file, start=1, stop=nc-n.gone)
          file.lbl <- paste(file.lbl, labels, sep="")
        }
        else
          file.lbl <- labels
      }
      else
        file.lbl <- "labels in second row of data file"
    }
  }

  if (!quiet) {
    max.chr <- nchar(file)
    if (!is.null(labels))
      if (nchar(file.lbl) > max.chr) max.chr <- nchar(file.lbl)
    .dash(max.chr + 14)
    cat("Data File:   ", file, "\n")
    if (!is.null(labels))  cat("Label File:  ", file.lbl, "\n")
    .dash(max.chr + 14)
  }

  # see if labels=="row2"
  if (is.null(labels))
    isnot.row2 <- TRUE
  else
    if (labels != "row2") isnot.row2 <- TRUE else isnot.row2 <- FALSE


  # do the read (into object d)
  # ---------------------------

  if (format=="fwd" || format=="csv") {  # text file

    if (format=="fwd")
      d <- utils::read.fwf(file=file, widths=widths, ...)

    else if (format=="csv") {
      line1 <- scan(file, what="character", nlines=1, sep="\t", quiet=TRUE)
      if (length(line1) > 1) {
        message(">>> A tab character detected in the first row of the data file.\n",
            "    Presume tab delimited data.\n", sep="")
        delim <- "\t"
      }
      else
        delim <- ","
      if (isnot.row2)  # read data
         d <- utils::read.csv(file=file, na.strings=missing, sep=delim, ...)
    }
  }  # end text file
  else if (format == "Excel") {
    txt <- "Hadley Wickham's readxl package]"
    cat("[with the read_excel function from", txt, "\n")
      if (isnot.row2)  # read data
        d <- readxl::read_excel(path=file, sheet=sheet)
        if (!is.null(list(...)$row.names))  #  see if do row.names
          d <- data.frame(d, row.names=list(...)$row.names)
          class(d) <- "data.frame"  # otherwise nonstandard class from read_excel
  }
  if (!is.null(labels)) {  # process labels
    if (format %in% c("fwd", "csv", "Excel")) {
      if (labels != "row2") {  # read labels file
        if (grepl(".xls$", file.lbl) || grepl(".xlsx$", file.lbl))
          format.lbl <- "Excel"
        else
          format.lbl <- "csv"
        if (format.lbl != "Excel")
          mylabels <- utils::read.csv(file=file.lbl, row.names=1, header=FALSE)
        else {
          mylabels <- readxl::read_excel(path=file.lbl, col_names=FALSE)
          mylabels <- data.frame(mylabels, row.names=1)
        }
        if (ncol(mylabels) == 1) names(mylabels) <- c("label")
        if (ncol(mylabels) == 2) names(mylabels) <- c("label", "unit")
      }
      else {  # labels == "row2"
        if (format != "Excel")
          mylabels <- utils::read.csv(file=file, nrows=1, sep=delim, ...)
        else {
          mylabels <- readxl::read_excel(path=file, ...)
          mylabels <- mylabels[1,]
        }
        var.names <- names(mylabels)
        mylabels <- data.frame(t(mylabels))  # var names are row names
        names(mylabels) <- "label"
        if (format != "Excel")
          d <- utils::read.csv(file=file, skip=1,
                           na.strings=missing, col.names=var.names, sep=delim, ...)
        else
          d <- readxl::read_excel(path=file, col_names=var.names)
        }
      # transfer labels and maybe units to data
      attr(d, which="variable.labels") <- as.character(mylabels$label)
      names(attr(d, which="variable.labels")) <- as.character(row.names(mylabels))
      if (ncol(mylabels) == 2) {
        attr(d, which="variable.units") <- as.character(mylabels$unit)
        names(attr(d, which="variable.units")) <- as.character(row.names(mylabels))
      }
    }
  }
else if (format == "SPSS")  # data and any labels
    d <- foreign::read.spss(file=file, to.data.frame=TRUE, ...)
  else if (format == "SAS") { # data
    d <- sas7bdat::read.sas7bdat(file=file, ...)
    txt <- "Matt Shotwell's sas7bdat package]"
    cat("[with the read.sas7bdat function from", txt, "\n")
  }
 else if (format == "R") {  # data and any labels
    x.env <- new.env()  # scratch environment
    load(file, envir=x.env)
    dname <- ls(x.env)
    d <- get(dname, pos=x.env)
  }
  else if (format == "SciencesPo") {  # data and any labels
    txt <- ifelse (substr(file,1,4) == "data", "", "data")
    file.name <- paste(txt, file, ".rda", sep="")

    path.name <- paste(find.package("SciencesPo"), "/data/",  file.name, sep="")
    if (!file.exists(path.name)) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "No SciencesPo data file with that name.\n\n",
        "To view the list of data files, enter  > help(SciencesPo)\n",
        "The data file names begin with 'data.'\n\n")
    }
    x.env <- new.env()  # scratch environment
    load(path.name, envir=x.env)

    dname <- paste(txt, file, sep="")
    d <- get(dname, pos=x.env)
  }
  # if a column is unique non-numeric, convert read as factor to character
  n.col <- apply(d, 2, function(x) sum(!is.na(x)))
  nu.col <- apply(d, 2, function(x) length(unique(na.omit(x))))
  fnu.col <- logical(length=ncol(d))
  if (format != "Excel") {
    for (i in 1:ncol(d)) if (nu.col[i]==n.col[i] && (is.factor(d[,i])))
          fnu.col[i] <- TRUE
    d[fnu.col] <- lapply(d[fnu.col], as.character)
  }
  else {  # read_excel does not convert strings to factors
    for (i in 1:ncol(d)) if (nu.col[i]!=n.col[i] && (is.character(d[,i])))
          fnu.col[i] <- TRUE
    d[fnu.col] <- lapply(d[fnu.col], as.factor)
  }
  # feedback
  # --------
  if (!quiet)
    head(d)
    # details(d, n.mcut, miss.zero, max.lines, miss.show,                  miss.matrix, brief)
  else
    cat("\n")
  return(d)

}

