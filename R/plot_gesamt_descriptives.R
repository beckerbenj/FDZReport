#' Create a line plot for absolute and cumulative frequencies.
#'
#' Create a line plot for absolute and cumulative frequencies.
#'
#'@param gesamt_descriptives An object as produced by \code{\link{calculate_gesamt_descriptives}}.
#'
#'@return A plot.
#'
#'@examples
#' # tbd
#'
#'@export
plot_gesamt_descriptives <- function(gesamt_descriptives, ylab) {
  by_var <- names(gesamt_descriptives)[1]
  ggplot2::ggplot(data = gesamt_descriptives, mapping = ggplot2::aes(x = .data[[by_var]],
                                                   y = .data$value, color = .data$Statistik, group = .data$Statistik)) +
    ggplot2::geom_line(linewidth = 1)  +
    ggplot2::ylab(ylab) +
    ggplot2::geom_text(mapping = ggplot2::aes(label = .data$value), hjust= 0.5, vjust = 2, size= 3.0, color= "black") +
    #geom_text(mapping = aes(y = cumFreq, label = cumFreq), hjust= 0.5, vjust = -2, size= 3.0, color= "black") +
    ggplot2::theme_bw() +
    #scale_x_continuous(breaks = seq(min(dscr_dat2$Jahr), max(dscr_dat2$Jahr))) +
    ggplot2::scale_y_continuous(breaks = seq(0, 800, by = 100))
}
