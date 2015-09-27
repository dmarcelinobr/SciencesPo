#' Frequency Plot of One or Two Variables
#'
#' Plots a bar chart of one or two variables with default colors,
#' including background color and grid lines, from a variety of
#' different types of data. For two variables a legend is also provided.
#'
#' @param x Variable(s) to analyze. Can be a single variable, either
#'  within a data frame or as a vector in the user's workspace.
#' @param by For each level of the first variable, x, display the frequencies at each level of this second variable, y.
#' @param data The data frame that contains the variables of interest, default is \code{.data}.
#' @param n.cat When analyzing all the variables in a data frame, specifies the largest number of unique values of variable of a numeric data type for which the variable will be analyzed as a categorical. Set to 0 to turn off.
#' @param col.fill Specified bar colors.
#' @param col.stroke Color of the border of the bars. Black by default unless the background is dark. Specify NA to remove the border.
#' @param col.bg Color of the plot background.
#' @param col.grid Color of the grid lines.
#' @param colors Optional palettes that provide more options, which include values of \code{"heat"}, \code{"rainbow"} and \code{"terrain"}.
#' @param random.col For two variable plots, when \code{TRUE}, randomizes the order of the colors within the chosen color palette, when the second variable is not ordered, otherwise is ignored. Each analysis generally yields different colors of the bars.
#' @param horiz By default bars are vertical, but can set this option to \code{TRUE}.
#' @param over.grid If \code{TRUE}, plot the grid lines over the histogram.
#' @param addtop When \code{horiz=FALSE}, in the same scale as the vertical axis, puts more space between the bars and the top of the plot area, usually to accommodate the legend when plotting two variables.
#' @param gap Gap between bars. Provides the value of the \code{space} option from the standard R \code{\link{barplot}} function with a default of 0.2 unless two variables are plotted and beside=\code{TRUE}, in which case the default is c(.1,1).
#' @param prop Display proportions instead raw frequencies.
#' @param xlab Label for x-axis, more generally the label for the values which could be on the vertical axis for a two variable plot if \code{horiz=TRUE}. Defaults to variable name.
#' @param ylab Label for y-axis, more generally the frequency axis. Defaults to Frequency.
#' @param main Title of graph.
#' @param cex.axis Scale magnification factor, which by default displays the axis values to be smaller than the axis labels.
#' @param col.axis Color of the font used to label the axis values.
#' @param beside For a two variable plot, set to \code{TRUE} for the levels of the first variable to be plotted as adjacent bars instead of stacked on each other.
#' @param col.low Only when the variable is an ordered factor, sets the color for the lowest level of the factor in the resulting ordered progression of colors.
#' @param col.hi Only when the variable is an ordered factor, sets the color for the highest level of the factor in the resulting ordered progression of colors.
#' @param count.levels If the name of a variable, this signals that the primary variable \code{x} has values that are counts, already tabulated, and that the specified variable here contains the names of the levels of x.
#' @param legend.title Title of the legend, which is usually set by default except when raw counts are entered as a matrix. Then a title must be specified to generate a legend.
#' @param legend.loc When plotting two variables, location of the legend, with the default in the right margin. Additional options from standard R are "topleft", "top", "topright" and others as shown in the help for the \code{\link{legend}} function.
#' @param legend.labels When plotting two variables, labels for the legend, which by default are the levels for the second or \code{by} variable.
#' @param legend.horiz By default the legend is vertical, but can be changed to horizontal.
#' @param quiet If set to \code{TRUE}, no text output. Can change system default with \code{\link{set}} function.
#' @param pdf.file Name of the pdf file to which graphics are redirected.
#' @param pdf.width Width of the pdf file in inches.
#' @param pdf.height}{Height of the pdf file in inches.
#' @param \dots Other parameter values for graphics as defined by \code{\link{barplot}}, \code{\link{legend}}, and  \code{\link{par}} including \code{space} for one variable only, \code{las=2} for vertical axis labels, and \code{cex.lab}, \code{col.main}, etc., and \code{col.ticks} to specify the color of the tick marks.
#' @examples
#' travel <- sample(c("Bike", "Bus", "Car", "Motorcycle"), size=25, replace=TRUE)
#' freq.plot(travel, horiz=TRUE)
#' @export
`freq.plot` <-
function(x=NULL, by=NULL, data=.data, n.cat=getOption("n.cat"),
       col.fill=NULL, col.stroke=getOption("col.stroke.bar"),
        col.bg=getOption("col.bg"),
        col.grid=getOption("col.grid"),
        random.col=FALSE,
         colors=c("rainbow", "terrain", "heat"),
         horiz=FALSE, over.grid=FALSE, addtop=1,
         gap=NULL, prop=FALSE,
         xlab=NULL, ylab=NULL, main=NULL,
         cex.axis=.85, col.axis="gray30",
         beside=FALSE, col.low=NULL, col.hi=NULL, count.levels=NULL,
         legend.title=NULL, legend.loc="right.margin", legend.labels=NULL,
         legend.horiz=FALSE,
          quiet=getOption("quiet"),
          pdf.file=NULL, pdf.width=5, pdf.height=5, ...)
        {


  if (missing(colors))
    colors <- getOption("colors")
  else
    colors <- match.arg(colors)

  if (missing(col.stroke))  # default black border unless dark bg
    if (sum(grDevices::col2rgb(col.bg))/3 > 80) col.stroke <- "black"

  x.name <- deparse(substitute(x))
  options(xname = x.name)

  df.name <- deparse(substitute(data))
  options(dname = df.name)


# -----------------------------------------------------------
# establish if a data frame, if not then identify variable(s)

  x.call <- NULL

  if (!missing(x)) {
    if (!exists(x.name, where=.GlobalEnv)) {  # x not in global env, in df
      .nodf(df.name)  # check to see if data frame container exists
      .xcheck(x.name, df.name, data)  # see if var in df, vars lists not checked
      vars.list <- as.list(seq_along(data))
      names(vars.list) <- names(data)
      x.col <- eval(substitute(x), envir=vars.list)  # col num of each var
      if (length(x.col) > 1) data <- data[, x.col]  # x is a vars list
      if (length(x.col) == 1) x.call <- eval(substitute(data$x))  # x is 1 var
    }
    else { # x is in the global environment (vector or data frame)
      if (is.data.frame(x))  # x a data frame
        data <- x
      else {  # x a vector in global
        x.call <- x
        if (is.function(x.call)) x.call <- eval(substitute(data$x))
      }
    }
  }


  if (!is.null(x.call)) {  # x is a single var, not a data frame

    # evaluate by
    if (!missing(by)) {

      # get actual variable name before potential call of data$x
      y.name <- deparse(substitute(by))
      options(yname = y.name)

      # see if y exists from a function call
      # indicate a function call with sys.nframe returns larger than 1
      if (exists(y.name, where=parent.frame(n=1)) && sys.nframe() > 1)
        in.call <- TRUE else in.call <- FALSE

      # get conditions and check for data existing
      if (!in.call) {
        xs <- .xstatus(y.name, df.name, quiet)
        in.global <- xs$ig
      }
      else in.global <- FALSE

      # see if var exists in data frame, if x not in Global Env or function call
      if (!in.global && !in.call) .xcheck(y.name, df.name, data)

      if (!in.global) y.call <- eval(substitute(data$by))
      else {  # vars that are function names get assigned to global
        y.call <- by
        if (is.function(y.call)) y.call <- eval(substitute(data$by))
      }

    }
    else
      y.call <- NULL


    # evaluate count.levels
    #---------------------
    if (!missing(count.levels)) {

      # get actual variable name before potential call of data$x
      count.levels.name <- deparse(substitute(count.levels))
      options(count.levelsname = count.levels.name)

      # get conditions and check for data existing
      xs <- .xstatus(count.levels.name, df.name, quiet)
      in.global <- xs$ig

      # see if var exists in data frame, if x not in Global Env or function call
      if (!missing(x) && !in.global)
        .xcheck(count.levels.name, df.name, data)

      if (!in.global) count.levels.call <- eval(substitute(data$count.levels))
      else {  # vars that are function names get assigned to global
        count.levels.call <- count.levels
        if (is.function(count.levels.call))
          count.levels.call <- eval(substitute(data$count.levels))
      }
    }
    else
      count.levels.call <- NULL


    if (length(unique(na.omit(x.call))) == 1) {
      cat("\n"); stop(call.=FALSE, "\n","------\n",
        "There is only one unique value for the values of ", x.name,
        ": ", na.omit(x.call)[1], "\n",
        "The bar chart is only computed if there is more than one",
        " unique value\n\n")
    }

# ---------------
# do the analysis

    .opendev(pdf.file, pdf.width, pdf.height)

    orig.params <- par(no.readonly=TRUE)
    on.exit(par(orig.params))

    bc <- .bc.main(x.call, y.call,
         col.fill, col.stroke, col.bg, col.grid, random.col, colors,
         horiz, over.grid, addtop, gap, prop, xlab, ylab, main,
         cex.axis, col.axis,  beside, col.low, col.hi,
         count.levels.call,
         legend.title, legend.loc, legend.labels, legend.horiz, quiet, ...)

    if (!is.null(pdf.file)) {
      dev.off()
      if (!quiet) .showfile(pdf.file, "freq.plot")
    }

    invisible(bc)
  }


  else
    bc.data.frame(data, n.cat,
      col.fill, col.stroke, col.bg, col.grid, random.col, colors,
      horiz, over.grid, addtop, gap, prop, xlab, ylab, main,
      cex.axis, col.axis,  beside, col.low, col.hi,
      count.levels,
      legend.title, legend.loc, legend.labels, legend.horiz, quiet,
      pdf.width, pdf.height, ...)

}



bc.data.frame <-
function(x, n.cat,
         col.fill, col.stroke, col.bg, col.grid, random.col, colors,
         horiz, over.grid, addtop, gap, prop, xlab, ylab, main,
         cex.axis, col.axis, beside, col.low, col.hi, count.levels,
         legend.title, legend.loc, legend.labels, legend.horiz, quiet,
         pdf.width, pdf.height, ...)  {

  for (i in 1:ncol(x)) {

    nu <- length(unique(na.omit(x[,i])))

    if (!is.numeric(x[,i]) || nu <= n.cat) {

      if (nlevels(factor(x[,i])) < length(x[,i])) {

        x.name <- names(x)[i]
        options(xname = x.name)

        if (nu == 1)
          cat("\nVariable", x.name, "has only one value. No barchart produced.\n\n")

        else {

          pdf.file <- paste("freq.plot_", x.name, ".pdf", sep="")
          .opendev(pdf.file, pdf.width, pdf.height)

          .bc.main(x[,i], by=NULL,
            col.fill, col.stroke, col.bg, col.grid, random.col, colors,
            horiz, over.grid, addtop, gap, prop, xlab, ylab, main,
            cex.axis, col.axis, beside, col.low, col.hi, count.levels,
            legend.title, legend.loc, legend.labels, legend.horiz, quiet,
            font.main=1, ...)

          dev.off()
          if (!quiet) .showfile(pdf.file, "bar chart")

        if (is.numeric(x[,i]) && nu <= n.cat)
          cat(">>> Variable is numeric, but only has", nu, "<= n.cat =", n.cat, "levels,",
              "so treat as a categorical variable.\n",
              "   To obtain the numeric summary, decrease  n.cat  to specify a",
              "lower number of unique values.\n",
              "   Suggest making this variable a factor with the R factor function.\n")
        }
      }

      else cat("\n", names(x)[i], "appears to contain unique Names or IDs", "\n")
    }

  }

}





