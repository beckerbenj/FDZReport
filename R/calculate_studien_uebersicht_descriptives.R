#' Calculate numbers of published studies.
#'
#' Calculate numbers of published studies.
#'
#'@param studien_input A \code{data.frame} as created by \code{\link{prepare_and_extract_studien}}.
#'
#'@return A plot.
#'
#'@examples
#' # tbd
#'
#'@export
calculate_studien_uebersicht_descriptives <- function(studien_input) {
  studien_dscr <- do.call(rbind, by(studien_input, studien_input$Jahr, function(subdat) {
    data.frame(Jahr = subdat[1, "Jahr"], Freq = nrow(subdat))
  }))
  studien_dscr$cumFreq <- cumsum(studien_dscr$Freq)
  studien_dscr
}
