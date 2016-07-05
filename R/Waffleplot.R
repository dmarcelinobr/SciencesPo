#' @title Make waffle (square pie) charts
#'
#' @description Given a named vector, this function will return
#' a ggplot object that represents a waffle chart of the values.
#'  The individual values will be summed up and each that will be the total number of squares in the grid.
#'
#' @note If the vector is not named or only partially named, capital letters will be
#' used instead. If you specify a string (vs \code{FALSE}) to \code{use_glyph} the function
#' will map the input to a FontAwesome glyph name and use that
#'  glyph for the tile instead of a block (making it more like an isotype pictogram than a
#' waffle chart). You'll need to actually install FontAwesome and use
#' the \code{extrafont} package (\code{https://github.com/wch/extrafont}) to
#' be able to use the FontAwesome glyphs.
#'
#' @param parts named vector of values to use for the chart
#' @param rows number of rows of blocks
#' @param xlab text for below the chart. Highly suggested this be used to
#'     give the "1 sq == xyz" relationship if it's not obvious
#' @param title chart title
#' @param colors exactly the number of colors as values in \code{parts}.
#'     If omitted, Color Brewer "Set2" colors are used.
#' @param size width of the separator between blocks (defaults to \code{2})
#' @param flip flips x & y axes
#' @param reverse reverses the order of the data
#' @param equal by default, waffle uses \code{coord_equal}; this can cause
#'     layout problems, so you an use this to disable it if you are using
#'     ggsave or knitr to control output sizes (or manually sizing the chart)
#' @param pad how many blocks to right-pad the grid with
#' @param use_glyph use specified FontAwesome glyph
#' @param glyph_size size of the FontAwesome font
#' @param legend_pos position of legend
#' @export
#' @examples
#' tiles <- c(One=80, Two=30, Three=20, Four=10)
#' Waffleplot(tiles, rows=8)
#'
#' Senate <- c(`Male (44%)`=44, `Female (56%)`=56)
#' Waffleplot(Senate, rows=10, size=0.5, colors=c("#af9139", "#544616"))
#'
Waffleplot <- function(parts, rows=10, xlab=NULL, title=NULL, colors=NA, size=2, legend_pos="right", flip=FALSE, reverse=FALSE, equal=TRUE, pad=0,
use_glyph=FALSE, glyph_size=12) {

  # fill in any missing names
  part_names <- names(parts)
  if (length(part_names) < length(parts)) {
    part_names <- c(part_names, LETTERS[1:length(parts)-length(part_names)])
  }

  names(parts) <- part_names

  # Use tableau20 if no colors are specified
  if (all(is.na(colors))) colors <- suppressWarnings(pub_color_pal("tableau10")(length(parts)))


  # make one big vector of all the bits
  parts_vec <- unlist(sapply(1:length(parts), function(i) {
    rep(names(parts)[i], parts[i])
  }))

  if (reverse) parts_vec <- rev(parts_vec)

  # setup the data frame for geom_rect
  dat <- base::expand.grid(y=1:rows, x=seq_len(pad + (ceiling(sum(parts) / rows))))

  # add NAs if needed to fill in the "rectangle"
  dat$.value <- c(parts_vec, rep(NA, nrow(dat)-length(parts_vec)))
  if(!inherits(use_glyph, "logical")){
    fontlab <- rep(fa_unicode[use_glyph],length(unique(parts_vec)))
    dat$fontlab <- c(fontlab[as.numeric(factor(parts_vec))], rep(NA, nrow(dat)-length(parts_vec)))
  }

  dat$.value <- factor(dat$.value, levels=part_names)

  if (flip) {
    gg <- ggplot2::ggplot(dat, aes(x=y, y=x))
  } else {
    gg <- ggplot2::ggplot(dat, aes(x=x, y=y))
  }

  gg <- gg + ggplot2::theme_bw()

  # make the plot

  if (inherits(use_glyph, "logical")) {

    gg <- gg + ggplot2::geom_tile(aes(fill=.value), color="white", size=size)
    gg <- gg + ggplot2::scale_fill_manual(name="",
                                 values=colors,
                                 labels=part_names,
                                 drop=FALSE)
    gg <- gg + ggplot2::guides(fill=guide_legend(override.aes=list(colour="#00000000")))
    gg <- gg + ggplot2::theme(legend.background=element_rect(fill="#00000000", color="#00000000"))
    gg <- gg + ggplot2::theme(legend.key=element_rect(fill="#00000000", color="#00000000"))

  } else {

    if (extrafont::choose_font("FontAwesome", quiet=TRUE) == "") {
      stop("FontAwesome not found. Install via: https://github.com/FortAwesome/Font-Awesome/tree/master/fonts",
           call.=FALSE)
    }

    suppressWarnings(
      suppressMessages(
        extrafont::font_import(system.file("fonts", package="SciencesPo"),
                    recursive=FALSE,
                    prompt=FALSE)))

    if (!(!interactive() || stats::runif(1) > 0.1)) {
      message("Font Awesome by Dave Gandy - http://fontawesome.io")
    }

    gg <- gg + ggplot2::geom_tile(color=NA, fill=NA, size=size, alpha=0, show.legend=FALSE)
    gg <- gg + ggplot2::geom_point(aes(color=.value), fill=NA, size=0, show.legend=TRUE)
    gg <- gg + ggplot2::geom_text(aes(color=.value,label=fontlab),
                         family="FontAwesome", size=glyph_size, show.legend=FALSE)
    gg <- gg + ggplot2::scale_color_manual(name="",
                                  values=colors,
                                  labels=part_names,
                                  drop=FALSE)
    gg <- gg + ggplot2::guides(color=guide_legend(override.aes=list(shape=15, size=7)))
    gg <- gg + ggplot2::theme(legend.background=element_rect(fill="#00000000", color="#00000000"))
    gg <- gg + ggplot2::theme(legend.key=element_rect(color="#00000000"))

  }

  gg <- gg + ggplot2::labs(x=xlab, y=NULL, title=title)
  gg <- gg + ggplot2::scale_x_continuous(expand=c(0, 0))
  gg <- gg + ggplot2::scale_y_continuous(expand=c(0, 0))

  if (equal) { gg <- gg + ggplot2::coord_equal() }

  gg <- gg + ggplot2::theme(panel.grid=element_blank())
  gg <- gg + ggplot2::theme(panel.border=element_blank())
  gg <- gg + ggplot2::theme(panel.background=element_blank())
  gg <- gg + ggplot2::theme(panel.margin=unit(0, "null"))

  gg <- gg + ggplot2::theme(axis.text=element_blank())
  gg <- gg + ggplot2::theme(axis.title.x=element_text(size=10))
  gg <- gg + ggplot2::theme(axis.ticks=element_blank())
  gg <- gg + ggplot2::theme(axis.line=element_blank())
  gg <- gg + ggplot2::theme(axis.ticks.length=unit(0, "null"))

  gg <- gg + ggplot2::theme(plot.title=element_text(size=18))

  gg <- gg + ggplot2::theme(plot.background=element_blank())
  gg <- gg + ggplot2::theme(plot.margin=unit(c(0, 0, 0, 0), "null"))
  gg <- gg + ggplot2::theme(plot.margin=rep(unit(0, "null"), 4))

  gg <- gg + ggplot2::theme(legend.position=legend_pos)
  gg
}
