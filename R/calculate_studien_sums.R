#' Calculate data request frequencies.
#'
#' Calculate data request frequencies.
#'
#'@param studien_descr_jahr_all An object as created by \code{\link{calculate_studien_descriptives}}.
#'
#'@return A plot.
#'
#'@examples
#' # tbd
#'
#'@export
calculate_studien_sums <- function(studien_descr_jahr_all) {
  studien_sums <- do.call(rbind, by(studien_descr_jahr_all, studien_descr_jahr_all$Studie, function(subdat) {
    data.frame(Studie = subdat[1, "Studie"], Freq = sum(subdat$Freq))
  }))

  # ordering
  studien_sums$Studie <- factor(studien_sums$Studie,
                                levels = studien_sums[order(studien_sums$Freq, decreasing = FALSE), "Studie"])
  # monitoring
  studien_sums$Monitoring <- ifelse(studien_sums$Studie %in% c("Plus", "PISA", "IQB-", "IGLU", "TIMSS",
                                                               "DESI"),
                                    yes = "Studien zum Bildungsmonitoring",
                                    no = "andere Studien")
  studien_sums
}
