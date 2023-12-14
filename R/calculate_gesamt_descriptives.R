#' Calculate data request frequencies.
#'
#' Calculate data request frequencies.
#'
#'@param input_list A \code{input_list} as created by \code{\link{prepare_and_merge_antraege}}.
#'@param by_var A character vector of length 1. Name of the variable.
#'
#'@return A plot.
#'
#'@examples
#' # tbd
#'
#'@export
calculate_gesamt_descriptives <- function(input_list, by_var) {
  antr <- input_list$antr

  new_var_name <- paste("pro", by_var)

  dscr_dat <- do.call(rbind, by(antr, antr[, by_var], function(subdat) {
    out <- data.frame(Halbjahr = subdat[1, by_var], nrow(subdat))
    names(out)[2] <- new_var_name
    out
  }))

  dscr_dat$kumulativ <- cumsum(dscr_dat[[new_var_name]])
  dscr_dat2 <- tidyr::pivot_longer(dscr_dat, cols = !tidyr::any_of(by_var), names_to = "Statistik")
  dscr_dat2[[by_var]] <- ordered(dscr_dat2[[by_var]])
  dscr_dat2
}
