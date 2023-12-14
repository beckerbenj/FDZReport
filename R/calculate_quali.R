#' Calculate qualification frequencies.
#'
#' Calculate data request frequencies for different qualification levels/phases.
#'
#'@param input_list A \code{input_list} as created by \code{\link{prepare_and_merge_antraege}}.
#'
#'@return A plot.
#'
#'@examples
#' # tbd
#'
#'@export
calculate_quali <- function(input_list) {
  antr <- input_list$antr

  suppressWarnings(qual_wide <- tidyr::separate(antr, col = tidyr::all_of("Qualifikation"), sep = "; ",
                                                into = paste0("Qualifikation_", 1:2)))
  qual_long <- tidyr::pivot_longer(qual_wide, col = !tidyr::any_of(c("Kurzname", "Jahr", "Studien", "Monat", "Halbjahr")),
                                   values_to = "Quali")
  qual_long <- qual_long[!is.na(qual_long$Quali), ]


  qual_dat <- do.call(rbind, by(qual_long, qual_long$Quali, function(subdat) {
    data.frame(Qualifikation = subdat[1, "Quali"], Freq = nrow(subdat))
  }))

  qual_dat2 <- qual_dat[!qual_dat$Quali %in% c("keine", "Forschungsprojekt"), ]
  qual_dat2$Quali <- eatTools::recodeLookup(qual_dat2$Quali,
                                            data.frame(old = c("Doktorarbeit", "Habilitationsschrift"),
                                                       new = c("Promotion", "Habilitation")))
  qual_dat2$Quali <- factor(qual_dat2$Quali, levels = c("Bachelorarbeit", "Masterarbeit", "Magisterarbeit", "Diplomarbeit",
                                                        "Promotion", "Habilitation"))

  qual_dat2$Prozent <- qual_dat2$Freq/sum(qual_dat2$Freq)
  qual_dat2$Prozent <- qual_dat2$Prozent * 100
  qual_dat2$label <- paste0(round(qual_dat2$Prozent, 1), "%")
  qual_dat2
}
