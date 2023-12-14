#' Create a line plot for absolute and cumulative frequencies.
#'
#' Create a line plot for absolute and cumulative frequencies.
#'
#'@param studien_descriptives An object as produced by \code{\link{calculate_studien_descriptives}}.
#'
#'@return A plot.
#'
#'@examples
#' # tbd
#'
#'@export
plot_studien_descriptives <- function(studien_descriptives) {

  ggplot2::ggplot(data = studien_descriptives, mapping = ggplot2::aes(x = .data$Jahr, colour = .data$Studie)) +
    #geom_line(mapping = aes(y = Freq), color = "darkblue", linewidth = 1) +
    ggplot2::geom_line(mapping = ggplot2::aes(y = .data$cumFreq), linewidth = 1) +
    ggplot2::ylab("Anzahl der Anträge") +
    #geom_text(mapping = aes(y = Freq, label = Freq), hjust= 0.5, vjust = 2, size= 3.0, color= "black") +
    ggplot2::geom_text(mapping = ggplot2::aes(y = .data$cumFreq, label = .data$cumFreq), hjust= 0.5, vjust = -2, size= 3.0, color= "black") +
    ggplot2::theme_bw() +
    ggplot2::scale_x_continuous(breaks = seq(min(studien_descriptives$Jahr), max(studien_descriptives$Jahr))) +
    ggplot2::scale_y_continuous(breaks = seq(0, 800, by = 100), limits = c(0, max(studien_descriptives$cumFreq, na.rm = TRUE) + 50)) +
    ggplot2::labs(title = "Kumulative Antragszahlen für Studien im Vergleich")
}
