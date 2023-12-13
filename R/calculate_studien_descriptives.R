#' Calculate data request frequencies separately for studies.
#'
#' Calculate data request frequencies separately for studies.
#'
#'@param input_list A \code{input_list} as created by \code{\link{prepare_and_merge_antraege}}.
#'@param wellen_separat A logical vector of length 1. Should different waves of longitudinal studies be counted multiple times if a
#'data request contains multiple ones?
#'@param studien_regex A character vector. Set of regular expressions selecting the studies which should be reported.
#'
#'@return A plot.
#'
#'@examples
#' # tbd
#'
#'@export
calculate_studien_descriptives <- function(input_list, wellen_separat = FALSE,
                                           studien_regex = c("PISA", "IQB-", "IGLU", "TIMSS", "BiKS", "ELEMENT", "DESI", "StEG")) {
  antr <- input_list[["antr"]]

  if(wellen_separat) {
    suppressWarnings(antr_wide <- tidyr::separate(antr, col = tidyr::all_of("Studien"), sep = ";", into = paste0("Studie_", 1:19)))
    #antr_long <- tidyr::pivot_longer(antr_wide, col = -c(Kurzname, Jahr, Qualifikation, Monat, Halbjahr), values_to = "Studie")
    antr_long <- tidyr::pivot_longer(antr_wide, col = !tidyr::any_of(c("Kurzname", "Jahr", "Qualifikation", "Monat", "Halbjahr")),
                                     values_to = "Studien")
    antr_long <- antr_long[!is.na(antr_long$Studien), ]
    antr <- antr_long
  }

  dscr_dat_list <- list()
  for(stud_snip in studien_regex) {
    stud_antr <- antr[grepl(stud_snip, antr$Studien), ]

    dscr_dat_list[[stud_snip]] <- do.call(rbind, by(stud_antr, stud_antr$Jahr, function(subdat) {
      data.frame(Jahr = subdat[1, "Jahr"], Freq = nrow(subdat))
    }))
    dscr_dat_list[[stud_snip]]$cumFreq <- cumsum(dscr_dat_list[[stud_snip]]$Freq)
  }

  dscr_dat_stud <- eatTools::do_call_rbind_withName(dscr_dat_list, col = "Studie")
  dscr_dat_stud
}
