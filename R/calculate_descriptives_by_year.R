#' Calculate numbers of published studies.
#'
#' Calculate numbers of published studies.
#'
#'@param input A \code{data.frame} as created by \code{\link{prepare_and_extract_studien}} or \code{\link{prepare_and_extract_cfs}}.
#'
#'@return A plot.
#'
#'@examples
#' # tbd
#'
#'@export
calculate_descriptives_by_year <- function(input) {

  dscr <- calculate_frequencies(input, by_var = "Jahr", new_var_name = "pro Jahr")
  pivot_longer_frequencies(dscr, exclude_col = "Jahr")
}
