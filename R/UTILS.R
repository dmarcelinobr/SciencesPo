#' @title Compute Weighted Correlations
#' @description Compute the weighted correlation.
#' @useDynLib SciencesPo
#' @export
#' @param x a matrix or vector to correlate with \code{y}.
#' @param y a matrix or vector to correlate with \code{x}. If \code{y} is NULL, \code{x} will be used instead.
#' @param weights an optional vector of weights to be used to determining the weighted mean and variance for calculation of the correlations.
#'
#' @examples
#'  x <- sample(10,10)
#'  y <- sample(10,10)
#'  w <- sample(5,10, replace=TRUE)
#'
#' WeightedCorrelation(x, y, w)
#'
`WeightedCorrelation` <- function(x, y = NULL, weights = NULL) {
  if (is.null(y)) {
    y <- x
  }
  q <- as.matrix(x)
  r <- as.matrix(y)
  if (is.null(weights)) {
    weights <- rep(1, dim(q)[1])
  }
  x <- q[!is.na(weights), ]
  y <- r[!is.na(weights), ]
  weights <- weights[!is.na(weights)]
  out <-
    .Call(
      "wcorr",
      as.matrix(x),
      as.matrix(y),
      as.double(weights),
      NAOK = TRUE,
      PACKAGE = "SciencesPo"
    )
  ## C code for this package was contributed by Marcus Schwemmle
  if (!is.null(colnames(x)))
    rownames(out) <- colnames(x)
  if (!is.null(colnames(y)))
    colnames(out) <- colnames(y)
  out
}
NULL




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




#' @title Render layer on top of a ggplot
#' @description Set up a rendering layer on top of a ggplot.
#' @param plot the plot to use as a starting point, either a ggplot2 or gtable.
#' @export
Render <- function(plot = NULL) {
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
