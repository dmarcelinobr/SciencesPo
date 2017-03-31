#' Pipe operator
#'
#' See \code{\link[magrittr]{\%>\%}} for more details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL



#' Compound pipe operator
#'
#' See \code{\link[magrittr]{\%<>\%}} for more details.
#'
#' @name %<>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %<>%
#' @usage lhs \%<>\% rhs
NULL



#' Determine if a vector is discrete.
#'
#' A discrete vector is a factor or a character vector
#'
#' @param x vector to test
#' @keywords internal
#' @export
#' @examples
#' is.discrete(1:10)
#' is.discrete(c("a", "b", "c"))
#' is.discrete(factor(c("a", "b", "c")))
is.discrete <- function(x) is.factor(x) || is.character(x) || is.logical(x)



#' Un-rowname.
#'
#' Strip rownames from an object
#'
#' @keywords internal
#' @param x data frame
#' @export
unrowname <- function(x) {
  rownames(x) <- NULL
  x
}


#' Check if a data frame is empty.
#'
#' Empty if it's null or it has 0 rows or columns
#'
#' @param df data frame to check
#' @keywords internal
#' @export
is.empty <- function(df) {
  (is.null(df) || nrow(df) == 0 || ncol(df) == 0)
}


#' Is a formula?
#' Checks if argument is a formula
#'
#' @keywords internal
#' @export
is.formula <- function(x) inherits(x, "formula")



#' @title Compute the Modal Value (Mode)
#' @description Compute the statistical mode.
#' @param x the object or variable.
#' @param na.rm a logical. Should NA values be removed?
#'
#' @examples
#'
#' modalValue(mtcars$mpg)
#'
#'@export
#'
`modalValue` <- function(x, na.rm=FALSE)
{
  # Determine the modal value of the data x.
  x = unlist(x)
  if(na.rm) x = x[!is.na(x)]
  u = unique(x)
  n = length(u)
  frequencies = rep(0, n)
  for(i in seq_len(n))
  {
    if(is.na(u[i]))
    {
      frequencies[i] = sum(is.na(x))
    } else
    {
      frequencies[i] = sum(x==u[i], na.rm=TRUE)
    }
  }
  u[which.max(frequencies)]
}
NULL






#' @title Re-sort Factor Levels by Frequency
#' @description Re-sort factor levels by frequency.
#' @param x the factor variable to be recoded.
#' @param decreasing Boolean. Whether to sort decreasing or
#' not.
#' @return the recoded factor variable.
#' @author Daniel Marcelino, \email{dmarcelino@@live.com}.
#' @examples
#' sex <- rep(c('m','f'), c(4, 2))
#' table(sex)
#'
#' table(Sortfactors(sex))
#'
#' @export
`Sortfactors` <- function(x, decreasing=TRUE) {
# TODO: add arbitrary factor levels from which to sort to.
  cc <- sort(table(x), decreasing=decreasing)
  return(factor(as.character(x), levels=names(cc)))
}






#' @encoding UTF-8
#' @title Some Formats for Nicer Display
#' @description Some predefined formats for nicer display.
#' @param x a numeric vector.
#' @param style a character name for style. One of "USD", "BRL", "EUR", "Perc".
#' @param digits an integer for the number of significant digits to be used for
#' numeric and complex x
#' @param flag a character string giving a format modifier as "-", "+", "#".
#' @param nsmall an integer for the minimum number of digits to the right of
#' the decimal point.
#' @param decimal.mark decimal mark style to be used with Percents (\%), usually (",") or (".").
#'
#' @examples
#' x <- as.double(c(0.1, 1, 10, 100, 1000, 10000))
#' Formatted(x)
#'
#' Formatted(x, "BRL")
#'
#' Formatted(x, "EUR")
#'
#' p = c(0.25, 25, 50)
#'
#' Formatted(p, "Perc", flag="+")
#'
#' Formatted(p, "Perc", decimal.mark=",")
#'
#' @export
`Formatted` <- function(x,
                        style = c("USD", "BRL", "EUR", "Perc"),
                        digits = 2,
                        nsmall = 2,
                        decimal.mark = getOption("OutDec"),
                        flag = "") {
  style <-
    .Match(arg = style,
           choices = c("usd", "brl", "eur", "perc"))

  if (style == "usd") {
    out <- paste("\u0024", formatC(x, digits = digits, format = "f"))
  }
  else if (style == "brl") {
    out <-
      paste(
        "\u0052\u0024",
        formatC(
          x,
          digits = digits,
          format = "f",
          big.mark = ".",
          decimal.mark = ","
        )
      )
  }
  else if (style == "eur") {
    out <-  paste("\u20ac", formatC(x, digits = digits, format = "f"))
  }
  else if (style == "perc") {
    out <-
      paste(
        formatC(
          x,
          digits = digits,
          decimal.mark =  decimal.mark,
          format = "f",
          flag = flag,
          drop0trailing = TRUE
        ),
        "\u0025",
        sep = ""
      )
  }
  else {
    warning(
      paste(style),
      " is not a valid style name. See `details` in the function documentation."
    )
  }
  out
}
NULL




#' @title Add Footnote to ggplot2 Objects
#' @description Add footnote to ggplot2 objects.
#'
#' @param note string or plotmath expression to be drawn.
#' @param x the x location of the label.
#' @param y the y location of the label.
#' @param hjust horizontal justification
#' @param vjust vertical justification
#' @param fontfamily the font family
#' @param fontface the font face ("plain", "bold", etc.)
#' @param color text color
#' @param size point size of text
#' @param angle angle at which text is drawn
#' @param lineheight line height of text
#' @param alpha the alpha value of the text
#' @param newlines should a new line be appended to the start and end
#' of the string, for spacing?
#'
#' @export
`Footnote` <-
  function(note,
           x = 0.85,
           y = 0.014,
           hjust = 0.5,
           vjust = 0.5,
           newlines = TRUE,
           fontfamily = "serif",
           fontface = "plain",
           color = "gray85",
           size = 9,
           angle = 0,
           lineheight = 0.9,
           alpha = 1)
  {
    text_par <- grid::gpar(
      col = color,
      fontsize = size,
      fontfamily = fontfamily,
      fontface = fontface,
      lineheight = lineheight,
      alpha = alpha
    )
    if(newlines)
      note <- paste0("\n", note, "\n")

    # render the note
    text.grob <-
      grid::textGrob(
        note,
        x = grid::unit(0.5, "npc"),
        y = grid::unit(0.5, "npc"),
        hjust = hjust,
        vjust = vjust,
        rot = angle,
        gp = text_par
      )
    annotation_custom(
      text.grob,
      xmin = x,
      xmax = x,
      ymin = y,
      ymax = y
    )
  }
NULL




#' @title Print the current date in a pretty format
#'
#' @description Print the current date in a pretty format.
#' @return string
#' @export
#'
#' @examples
#' Today()

`Today` <- function() {
  d <- date()
  month <- substr(d,5,7)
  day <- substr(d,9,10)
  year <- substr(d,21,24)
  paste(day,month,year)
}
NULL




#' @title Render layer on top of a ggplot
#' @description Set up a rendering layer on top of a ggplot.
#' @param plot the plot to use as a starting point, either a ggplot2 or gtable.
#' @export
`Render` <- function(plot = NULL) {
  d <- data.frame(x = 0:1, y = 0:1)  # dummy data
  p <- ggplot(d, aes_string(x = "x", y = "y")) + # empty plot
    scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    theme_blank() + # with empty theme
    labs(x = NULL, y = NULL)  # and absolutely no axes
  if (!is.null(plot)) {
    g <- .ggplot_to_gtable(plot)
    plot.grob <- grid::grobTree(g)
    p <- p + annotation_custom(plot.grob)
  }
  p
}
NULL





#' Clean up a character vector to make it numeric
#'
#' Remove commas and whitespace, primarily
#'
#' @param x character vector to process
#' @return numeric vector
#' @keywords Clean-up
#' @export
AsNumeric <- function(x) { as.numeric(gsub(",", "", trimws(x))) }

#' Clean up a character vector to make it a percent
#'
#' Remove "%" primarily, convert to numeric and divide by 100
#'
#' @param x character vector to process
#' @return numeric vector
#' @keywords Clean-up
#' @export
AsShare <- function(x) { as.numeric(gsub("%", "", trimws(x))) / 100 }






