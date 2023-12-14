#' Calculate numbers of campus file requests.
#'
#' Calculate numbers of campus file requests.
#'
#'@param studien_input A \code{data.frame} as created by \code{\link{prepare_and_extract_studien}}.
#'
#'@return A plot.
#'
#'@examples
#' # tbd
#'
#'@export
calculate_cfs_descriptives <- function(cfs_input) {
  studien_dscr <- do.call(rbind, by(cfs_input, cfs_inputt$Jahr, function(subdat) {
    data.frame(Jahr = subdat[1, "Jahr"], Freq = nrow(subdat))
  }))
  studien_dscr$cumFreq <- cumsum(studien_dscr$Freq)
  studien_dscr
}
