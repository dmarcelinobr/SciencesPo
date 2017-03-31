#' @encoding UTF-8
#' @title Summary Statistics Table
#'
#' @description Produce summary description table of a data frame with the following features:
#' variable names, labels, factor levels, frequencies or summary statistics.
#'
#' @param .data a data frame.
#' @param style the style to be used in pander table, one of \dQuote{multiline},
#' \dQuote{grid}, \dQuote{simple}, and \dQuote{rmarkdown}.
#' @param justify \pkg{pander} argument; defaults is justified to \dQuote{left}.
#' @param max.number an integer for the maximum number of items to be displayed in the frequency cell. If variable has more distinct values, no frequency will be shown (only a message stating the number of distinct values).
#' @param trim.strings remove white spaces at the beginning or end of the string. This may impact the frequencies, so interpret the frequencies cell accordingly. Defaults to \code{FALSE}.
#' @param max.width Limits the number of characters to display in the frequency tables. Defaults to \code{15}.
#' @param digits the number of digits for rounding.
#' @param split.cells \pkg{pander} argument. Number of characters allowed on a
#' line before splitting the cell. Defaults to 35.
#' @param display.labels If \code{TRUE}, variable labels will be displayed. Defaults to \code{FALSE}.
#' @param display.attributes If \code{TRUE}, variable attibutes will be displayed. Defaults to \code{FALSE}.
#' @param file the text file to be written to disk. Defaults to \code{NA}.
#' @param append When \dQuote{file} argument is supplied, this indicates whether to append output to existing file (\code{TRUE}) or to overwrite any existing file (\code{FALSE}, default). If \code{TRUE} and no file exists, a new file will be created.
#' @param escape.pipe Only useful when \code{style='grid'} and \code{file} argument is not \code{NA}, in which case it will escape the pipe character (|) to allow Pandoc to correctly convert multiline cells.
#' @param \dots additional arguments passed to \pkg{pander}.
#' @details The IQR formula used is the R standard \code{IQR(x) = quantile(x, 3/4) - quantile(x, 1/4)}. The (CV) stands for the
#' coefficient of variation also known as relative standard deviation (RSD),
#' defined as the ratio of the standard deviation
#' to the mean.
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}
#' @keywords Exploratory
#' @examples
#' data(religiosity)
#' Describe(religiosity)
#'
#' @rdname Describe
#' @aliases Detail
#' @export
`Describe` <- function(.data, style="multiline", justify="left",
                      max.number=10,
                      trim.strings=FALSE,
                      max.width=15,
                      digits=2,
                      split.cells=35,
                      display.labels=FALSE,
                      display.attributes=FALSE,
                      file=NA, append=FALSE,
                      escape.pipe=FALSE, ...) {

  df.convers <- FALSE
  if(!is.data.frame(.data)) {
    .data <- try(as.data.frame(.data))
    if(inherits(.data, "try-error")) {
      message(".data is not a dataframe and attempted conversion failed")
    } else {
      message(".data was converted to a dataframe")
      df.convers <- TRUE # in this case df.name will be taken from var.name
    }
  }

  # create an output dataframe
  output <- data.frame(Variable=character(),
                       Label=character(),
                       Attributes=character(),
                       Levels.or.Stats=character(),
                       Freq=character(),
                       Valid=numeric(),
                       stringsAsFactors=FALSE)

  # Set additional attributes
  class(output) <- c("SciencesPo", class(output))
  attr(output, "scpo.type") <- "Describe"

  # use the parsing function to identify particularities such as row indexing
  tmp.attr <- .parse.arg(as.character(match.call()[2]))
  for(item in names(tmp.attr)) {
    attr(output, item) <- tmp.attr[[item]]
  }

  if(df.convers) {
    attr(output, "df.name") <- attr(output, "var.name")
    attr(output, "notes") <- paste(attr(output, "df.name"), "was converted from",
                                   typeof(.data), "to data.frame")
    attr(output, "var.name") <- NULL
  }

  attr(output, "n.obs") <- nrow(.data)
  attr(output, "date") <- Sys.Date()
  attr(output, "pander.args") <- list(style = style, round = digits, justify = justify,
                                      split.table = Inf, keep.line.breaks = TRUE,
                                      split.cells = split.cells, ...=...)

  # iterate over dataframe columns
  for(i in seq_len(ncol(.data))) {

    # extract data
    column.data <- .data[[i]]

    # Add column name
    output[i,1] <- paste(i, names(.data)[i], sep=".\n")

    # Add column label (if applicable)
    if(display.labels)
      output[i,2] <- Label(.data[i])

    # Add variable attributes (typeof, class)
    if(display.attributes)
    output[i,3] <- paste("type:",typeof(column.data),
                         "\nclass:",paste(class(column.data),collapse="\n + "),sep="")

    # For factors, display a column of levels and a column of frequencies
    if(is.factor(column.data)) {
      n.levels <- nlevels(column.data)
      if(n.levels <= max.number) {
        output[i,4] <- paste(1:n.levels,". ", levels(column.data), collapse="\n", sep="")

        fr <- table(column.data,useNA="no") # raw freqs
        pct <- round(prop.table(fr)*100,1) # percentage
        names(fr) <- 1:n.levels
        output[i,5] <- paste(names(fr),": ", fr," (",pct,"%)",sep="",collapse="\n")
      } else {
        output[i,4] <- paste(1:max.number,". ", levels(column.data)[1:max.number], collapse="\n", sep="")
        output[i,4] <- paste(output[i,4], paste(n.levels - max.number, "Other levels (not displayed)"),sep="\n")
        output[i,5] <- paste(as.character(length(unique(column.data))),"distinct val")
      }
    }

    # For numeric data, display a column of descriptive stats and a column of frequencies
    else if(is.numeric(column.data)) {
      output[i,4] <- paste("mean (sd) = ",round(mean(column.data, na.rm = TRUE),digits),
                           " (",round(sd(column.data, na.rm = TRUE),digits), ")\n",
                           "min < med < max = ", round(min(column.data,  na.rm = TRUE),digits),
                           " < ", round(median(column.data,  na.rm = TRUE),digits),
                           " < ", round(max(column.data,  na.rm = TRUE),digits),"\n",
                           "IQR (CV) = ", round(stats::IQR(column.data,na.rm=TRUE),digits),
                           " (", round(sd(column.data, na.rm = TRUE)/mean(column.data, na.rm = TRUE),digits),
                           ")", collapse="",sep="")

      if(length(unique(column.data)) <= max.number) {
        fr <- table(column.data,useNA="no") # raw freqs
        pct <- round(prop.table(fr)*100,1)
        output[i,5] <- paste(round(as.numeric(names(fr)),digits),": ", fr," (",pct,"%)",sep="",collapse="\n")
      }
      else {
        output[i,5] <- paste(as.character(length(unique(column.data))),"distinct val")
      }
    }

    # For text data, skip a column and display a column of frequencies
    else if(is.character(column.data)) {
      output[i,4] <- ""

      if(trim.strings)
        column.data.tmp <- sub(pattern = "\\A\\s*(.+?)\\s*\\z", replacement="\\1", x=column.data, perl=TRUE)
      else
        column.data.tmp <- column.data

      if(length(unique(column.data.tmp)) <= max.number) {
        fr <- table(column.data.tmp,useNA = "no")
        pct <- round(prop.table(fr)*100,1)
        output[i,5] <- paste(substr(names(fr),1,max.width),": ",
                             fr," (",pct,"%)",sep = "",collapse="\n")
      }
      else output[i,5] <- paste(as.character(length(unique(column.data.tmp))),"distinct val")
    }

    # For data that does not fit in previous categories (numeric, character, factor)
    else {
      output[i,4] <- ""
      if(length(unique(column.data)) <= max.number) {
        fr <- table(column.data,useNA = "no")
        pct <- round(prop.table(fr)*100,1)
        output[i,5] <- paste(substr(names(fr),1,max.width),": ",
                             fr," (",pct,"%)",sep = "",collapse ="\n")
      }
      else output[i,5] <- paste(as.character(length(unique(column.data))),"distinct val")
    }

    # Add valid data info
    n.nas <- sum(is.na(column.data))
    n.val <- nrow(.data) - n.nas

    output[i,6] <- paste(n.val,"/", nrow(.data),"\n",
                         "(", format(n.val/nrow(.data)*100, digits = 0, nsmall=1), "%)",
                         sep="", collapse = "\n")
  }

  if(!display.labels)
    output$Label <- NULL

  if(!display.attributes)
    output$Attributes <- NULL

  # Escape pipes when needed (this is for Pandoc to correctly handle grid tables with multiline cells)

  if(!is.na(file)) {

    if(style=="grid" && escape.pipe) {
      output.esc.pipes <- paste(gsub(".\\|","\\\\|",utils::capture.output(output)), collapse="\n")
      utils::capture.output(cat(output.esc.pipes), file = file, append = append)
    }
    else if(grepl("\\.html$",file)) {
      if(isTRUE(append)) message("Append is not supported for html files. This parameter will be ignored")
      file.copy(from=print(output, method="browser",open=FALSE),to = normalizePath(file, mustWork = FALSE))
    } else {
      utils::capture.output(output, file = file, append = append)
    }
    message("Output successfully written to file ", normalizePath(file))
    return(invisible(output))
  }
  return(output)
}
