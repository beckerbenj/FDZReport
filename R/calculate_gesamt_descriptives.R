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

  dscr_dat <- calculate_frequencies(antr, by_var = by_var, new_var_name = new_var_name)

  pivot_longer_frequencies(dscr_dat, exclude_col = by_var, names_to = "Statistik")
}

calculate_frequencies <- function(dat, by_var, new_var_name) {
  dscr_list <- by(dat, dat[, by_var], function(subdat) {
    out <- data.frame(subdat[1, by_var], nrow(subdat))
    names(out) <- c(by_var, new_var_name)
    out
  })
  dscr_dat <- do.call(rbind, dscr_list)
  dscr_dat$kumulativ <- cumsum(dscr_dat[[new_var_name]])
  dscr_dat
}

pivot_longer_frequencies <- function(dat, exclude_col, names_to = "Statistik") {
  dat2 <- tidyr::pivot_longer(dat, cols = !tidyr::any_of(exclude_col), names_to = names_to)
  dat2[[exclude_col]] <- ordered(dat2[[exclude_col]])
  dat2
}
