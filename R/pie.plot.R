if(getRversion() >= "2.15.1") globalVariables(c(".data"))

#' @title  Creates a pie chart using ggplot2.
#'
#' @description  Use pie charts with care. See http://www.edwardtufte.com/bboard/q-and-a-fetch-msg?msg_id=00018S
#' on Edward Tufte's website for good arguments against the use of pie charts.
#' For a contrary point-of-view, see Spence's article, No Humble Pie: The Origins and
#' Usage of a Statistical Chart (http://www.psych.utoronto.ca/users/spence/Spence%202005.pdf).
#'
#' @param .data the data frame.
#' @param var the name of the column to generate the pie chart for.
#' @param label The label for the legend.
#' @export
pie.plot <- function(.data, var, label=var) {
 # res <- dplyr::count(.data, ...)
  .data$pie = .data[,var]
  l = levels(.data$pie)
  t = table(.data$pie)
  levels(.data$pie) = paste(names(t), " ", format(100*t/sum(t),digits=1), "%", sep="")
  p = ggplot2::ggplot(.data, ggplot2::aes(x=factor(1), fill=pie)) +
    ggplot2::geom_bar(width=1) +
    ggplot2::coord_polar(theta="y") +
    ggplot2::xlab("") + ggplot2::ylab("") +
    ggplot2::scale_fill_hue(name=label, breaks=levels(.data$pie), labels=levels(.data$pie)) + theme_pub() +
    theme(axis.text=ggplot2::element_blank(), axis.ticks=ggplot2::element_blank())
  print(p)
}
