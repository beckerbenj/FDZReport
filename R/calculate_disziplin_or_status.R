#' Calculate discipline or status frequencies.
#'
#' Calculate data request frequencies for different scientific disciplines or status levels.
#'
#'@param input_list A \code{input_list} as created by \code{\link{prepare_and_merge_antraege}}.
#'@param disziplin_or_status A character vector of length 1. Should \code{"disziplin"} or \code{"status"} frequencies be calculated?.
#'
#'@return A plot.
#'
#'@examples
#' # tbd
#'
#'@export
calculate_disziplin_or_status <- function(input_list, disziplin_or_status) {
  betei <- input_list$betei

  test_split <- strsplit(betei$Anträge, split = "; ")
  max_splits <- max(sapply(test_split, length))

  suppressWarnings(betei_wide <- tidyr::separate(betei, col = tidyr::all_of("Anträge"), sep = "; ",
                                                 into = paste0("Antrag_", seq(max_splits))))
  betei_long <- tidyr::pivot_longer(betei_wide, col = !tidyr::any_of(c("Disziplin", "Status")),
                                    values_to = "Antrag")
  betei_long <- betei_long[!is.na(betei_long$Antrag), ]

  test_split <- strsplit(betei$Disziplin, split = "; ")
  max_splits <- max(sapply(test_split, length))

  suppressWarnings(betei_wide2 <- tidyr::separate(betei, col = tidyr::all_of("Disziplin"), sep = "; ",
                                                  into = paste0("Disziplin_", seq(max_splits))))
  betei_long2 <- tidyr::pivot_longer(betei_wide2, col = !tidyr::any_of(c("Anträge", "Status", "Name")),
                                     values_to = "Disziplin")
  betei_long2 <- betei_long2[!is.na(betei_long2$Disziplin), ]

  betei_long2$Disziplin <- eatTools::recodeLookup(betei_long2$Disziplin,
                                                  data.frame(old = c("Bildungswissenschaften", "Sozialwissenschaften"),
                                                             new = c("Erziehungswissenschaften", "Soziologie")))

  out_dat <- do.call(rbind, by(betei_long2, betei_long2[[disziplin_or_status]], function(subdat) {
    data.frame(wiss.Status = subdat[1, disziplin_or_status], Freq = nrow(subdat))
  }))

  out_dat$Prozent <- out_dat$Freq/sum(out_dat$Freq) * 100
  out_dat$label <- paste0(round(out_dat$Prozent, 1), "%")
  out_dat
}
