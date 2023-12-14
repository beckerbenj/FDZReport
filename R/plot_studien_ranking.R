#' Create a bar plot with rank ordered cumulative frequencies.
#'
#' Create a bar plot with rank ordered cumulative frequencies.
#'
#'@param studien_sums An object as produced by \code{\link{calculate_studien_sums}}.
#'
#'@return A plot.
#'
#'@examples
#' # tbd
#'
#'@export
plot_studien_ranking <- function(studien_sums) {
  ggplot2::ggplot(data = studien_sums[studien_sums$Freq > 10, ],
         mapping = ggplot2::aes(x = .data$Studie, y = .data$Freq, fill = .data$Monitoring)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_text(ggplot2::aes(label = .data$Freq), vjust = 0, hjust = -0.5) +
    ggplot2::coord_flip() +
    ggplot2::ylab("Häufigkeit der Beantragung") +
    ggplot2::xlab("") +
    ggplot2::scale_fill_manual(values = c("Studien zum Bildungsmonitoring" = "darkred", "andere Studien" = "darkblue")) +
    ggplot2::ylim(c(0, max(studien_sums$Freq, na.rm = TRUE) + 50)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "top") +
    ggplot2::theme(legend.title = ggplot2::element_blank())
}
