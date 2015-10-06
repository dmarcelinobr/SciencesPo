#' Make a boxplot
#'
#' Uses the standard R boxplot function, \code{\link{boxplot}} to display a boxplot in color. Also display the relevant statistics such as the hinges, median and IQR.
#'
#' @param x Variable(s) to analyze. Can be a single variable, either
#'  within a data frame or as a vector in the user's workspace.
#' @param data The data frame that contains the variables of interest, default is \code{.data}.
#' @param n.cat When analyzing all the variables in a data frame, specifies the largest number of unique values of variable of a numeric data type for which the variable will be analyzed as a categorical. Set to 0 to turn off.
#' @param col.fill Color of the box.
#' @param col.stroke Color of any points that designate outliers. By default this is the same color as the box.
#' @param col.bg Color of the plot background.
#' @param col.grid Color of the grid lines.
#' @param cex.axis Scale magnification factor, which by defaults displays the axis values to be smaller than the axis labels. Provides the functionality of, and can be replaced by, the standard R \code{cex.axis.}
#' @param col.axis Color of the font used to label the axis values.
#' @param xlab Label for the value axis, which defaults to the variable's name.
#' @param main Title of graph.
#' @param digits.d Number of decimal digits displayed in the listing of the summary statistics.
#' @param horiz Orientation of the boxplot. Set \code{FALSE} for vertical.
#' @param add.points If \code{TRUE}, then place a dot plot (i.e., stripchart) over the box plot.
#' @param quiet If set to \code{TRUE}, no text output. Can change system default with \code{\link{set}} function.
#' @param pdf.file Name of the pdf file to which graphics are redirected. If there is no filetype of \code{.pdf}, the filetype is added to the name.
#' @param pdf.width Width of the pdf file in inches.
#' @param pdf.height Height of the pdf file in inches.
#' @param \dots Other parameter values for graphics as defined processed by \code{\link{boxplot}} such as \code{whiskcol} for the whisker color, etc. and \code{\link{par}}, including \code{ylim} to set the limits of the value axis, \code{lwd} for the line width, \code{cex.lab} for the size of the label, \code{col.main} for the title, etc., and \code{col.ticks} to specify the color of the tick marks.
#'
#' @examples
#' y <- rnorm(100,50,10)
#' y[1] <- 90
#'
#' box.plot(y)
#'
#' @export
 box.plot <-
function(x=NULL, data=.data, n.cat=getOption("n.cat"),

    col.fill=getOption("col.fill.bar"),
    col.stroke=getOption("col.stroke.bar"),
    col.bg=getOption("col.bg"),
    col.grid=getOption("col.grid"),

    cex.axis=.85, col.axis="gray30",
    xlab=NULL, main=NULL, digits.d=NULL,

    horiz=TRUE, add.points=FALSE,

    quiet=getOption("quiet"),
    pdf.file=NULL, pdf.width=5, pdf.height=5, ...) {

  if (getOption("colors") == "gray") col.stroke <- "black"
  if (getOption("colors") == "gray.black") col.stroke <- getOption("col.stroke.pt")

  # get actual variable name before potential call of data$x
  x.name <- deparse(substitute(x))
  options(xname = x.name)

  df.name <- deparse(substitute(data))
  options(dname = df.name)

  pdf.nm <- FALSE
  if (!missing(pdf.file)) pdf.nm <- TRUE

# -----------------------------------------------------------
# establish if a data frame, if not then identify variable(s)

  if (!missing(x)) {

    if (!exists(x.name, where=.GlobalEnv)) {  # x not in global env, in df
      .nodf(df.name)  # check to see if data frame container exists
      .xcheck(x.name, df.name, data)  # var in df?, vars lists not checked
      all.vars <- as.list(seq_along(data))  # even if only a single var
      names(all.vars) <- names(data)  # all data in data frame
      x.col <- eval(substitute(x), envir=all.vars)  # col num selected vars
      if (class(data) != "list") {
        data <- data[, x.col]
        if (length(x.col) == 1) {  # x is 1 var
          data <- data.frame(data)
          names(data) <- x.name
         }
      }
      else {  # class of data is "list"
        data <- data.frame(data[[x.col]])
        names(data) <- x.name
      }
    }

    else { # x is in the global environment (vector or data frame)
      if (is.data.frame(x))  # x a data frame
        data <- x
      else {  # x a vector in global
        if (!is.function(x))
          data <- data.frame(x)  # x is 1 var
        else
          data <- data.frame(eval(substitute(data$x)))  # x is 1 var
        names(data) <- x.name
      }
    }

  }


# ---------------
# do the analysis

  go.pdf <- FALSE
  if (pdf.nm || ncol(data) > 1) go.pdf <- TRUE

  for (i in 1:ncol(data)) {

    nu <- length(unique(na.omit(data[,i])))

    x.name <- names(data)[i]
    options(xname = x.name)

    if (is.numeric(data[,i])) {
      if (nu > n.cat) {

      pdf.fnm <- .pdfname("boxplot", x.name, go.pdf, pdf.nm, pdf.file)
     .opendev(pdf.fnm, pdf.width, pdf.height)

      stuff <- .bx.main(data[,i], col.fill, col.stroke, col.bg, col.grid,
         cex.axis, col.axis,
         horiz, add.points, xlab, main, digits.d, quiet, ...)
      txsts <- stuff$tx
      if (length(txsts)==0) txsts <- ""

      txotl <- ""
      if (!quiet) {
        txotl <- .outliers2(data[,i])
        if (length(txotl)==0) txotl <- "No outliers"
      }

      if (ncol(data) > 1) {  # for a variable range, just the text output
        class(txsts) <- "out_piece"
        class(txotl) <- "out_piece"
        output <- list(out_stats=txsts, out_outliers=txotl)
        class(output) <- "out_all"
        print(output)
      }

      if (go.pdf) {
        grDevices::dev.off()
        if (!quiet) .showfile(pdf.fnm, "Box Plot")
      }

    }  # nu > n.cat
    else
      .ncat("Box Plot", x.name, nu, n.cat)

    }  # is.numeric(data[,i])
  }  # for


  grDevices::dev.set(which=2)  # reset graphics window for standard R functions

  if (ncol(data)==1  &&  nu>n.cat) {

    class(txsts) <- "out_piece"
    class(txotl) <- "out_piece"

    output <- list(type="box.plot",
      out_stats=txsts, out_outliers=txotl,
      n=stuff$n, n.miss=stuff$n.miss, min=stuff$mn, lower_whisker=stuff$lw,
      lower_hinge=stuff$lh, median=stuff$md, upper_hinge=stuff$uh,
      upper_whisker=stuff$uw, max=stuff$mx, IQR=stuff$IQR)

    class(output) <- "out_all"

    return(output)

  }

}


