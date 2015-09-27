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

          pdf.file <- paste("BarChart_", x.name, ".pdf", sep="")
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
