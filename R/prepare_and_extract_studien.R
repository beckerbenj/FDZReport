#' Prepare and merge data of data requests.
#'
#' Load, prepare and merge data of data requests.
#'
#'@param studien_uebersicht A \code{.xlsm} file including all archieved studies at the FDZ, as produced by \code{StudienDB}.
#'
#'@return A plot.
#'
#'@examples
#' # tbd
#'
#'@export
prepare_and_extract_studien <- function(studien_uebersicht) {
  studien_ori <- readxl::read_xlsx(studien_uebersicht, sheet = "Studien")
  studien <- studien_ori[studien_ori$Status == "5. Veröffentlicht", c("Studienname", "verfügbar seit")]

  #table(studien_ori$Status)
  studien$Jahr <- as.numeric(substr(studien$`verfügbar seit`, 7, 10))

  stopifnot(all(!is.na(studien$Jahr)))
  studien$Studienname <- gsub("v.$", "", studien$Studienname)
  studien[!duplicated(studien$Studienname), ]
}
