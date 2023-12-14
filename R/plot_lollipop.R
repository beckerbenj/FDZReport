#' Create a lollipop plot.
#'
#' Create a lollipop plot.
#'
#'@param dat A \code{data.frame}.
#'@param x Character vector of length 1. Name of the column of the grouping variable.
#'@param y Character vector of length 1. Name of the column of the relative frequency variable.
#'@param label Character vector of length 1. Name of the column containing the labeling variable.
#'@param x_axis_label Character vector of length 1. Label for the x axis.
#'
#'@return A plot.
#'
#'@examples
#' # tbd
#'
#'@export
plot_lollipop <- function(dat, x, y = "Prozent", label = "label", x_axis_label) {
  ggplot2::ggplot(data = dat, mapping = ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
    ggplot2::geom_point() +
    ggplot2::geom_segment(ggplot2::aes(x = .data[[x]], xend = .data[[x]], y = 0, yend = .data[[y]])) +
    ggplot2::geom_text(ggplot2::aes(label = .data[[label]]), vjust=0, hjust = -0.5) +
    ggplot2::ylim(c(0, max(dat[[y]] + 5))) +
    ggplot2::theme_bw() +
    ggplot2::coord_flip() +
    ggplot2::scale_x_discrete(name = x_axis_label)
}
