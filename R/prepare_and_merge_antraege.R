#' Prepare and merge data of data requests.
#'
#' Load, prepare and merge data of data requests.
#' Creation date of \code{antraege_aktiv} und \code{antraege_archiv} is compared and if \code{difftime}
#' is larger than 1 day, an error is issued.
#'
#'@param antraege_aktiv A \code{.xlsm} file including all active data requests, as produced by \code{FAMe}.
#'@param antraege_archiv A \code{.xlsm} file including all archived data requests, as produced by \code{FAMe}.
#'
#'@return A plot.
#'
#'@examples
#' # tbd
#'
#'@export
prepare_and_merge_antraege <- function(antraege_aktiv, antraege_archiv) {
  antr <- readxl::read_xlsx(antraege_aktiv, sheet = "Anträge")
  antr_arch <- readxl::read_xlsx(antraege_archiv, sheet = "Anträge")

  ctime_aktiv <- file.info(antraege_aktiv)$ctime
  ctime_archiv <- file.info(antraege_archiv)$ctime
  creation_diff <- difftime(ctime_aktiv, ctime_archiv, units = "days")
  if(abs(creation_diff) > 1) stop("'antraege_aktiv' and 'antraege_archiv' have been created on different days. Both data base outputs should be created at the exact same time.")


  checkmate::assert_names(names(antr), must.include = c("Kurzname", "Status", "Studien", "Qualifikation"))
  checkmate::assert_names(names(antr_arch), must.include = c("Kurzname", "Status", "Studien", "Qualifikation"))

  betei <- readxl::read_xlsx(antraege_aktiv, sheet = "Beteiligte")
  betei_arch <- readxl::read_xlsx(antraege_archiv, sheet = "Beteiligte")

  checkmate::assert_names(names(betei), must.include = c("Name", "Anträge", "Disziplin", "Status"))
  checkmate::assert_names(names(betei_arch), must.include = c("Name", "Anträge", "Disziplin", "Status"))

  antr_m <- rbind(antr[, c("Kurzname", "Status", "Studien", "Qualifikation")],
                  antr_arch[, c("Kurzname", "Status", "Studien", "Qualifikation")])
  stopifnot(length(unique(antr_m$Kurzname)) == nrow(antr_m))

  ### Antragsselektion
  antr_m2 <- antr_m[!antr_m$Status %in% c("9. Retour") & !grepl("^LP", antr_m$Kurzname), c("Kurzname", "Studien", "Qualifikation")]

  ### Aufbereitung Zeitpunkt
  antr_m2$Jahr <- substr(antr_m2$Kurzname, start = 1, stop = 2)
  antr_m2$Jahr <- as.numeric(paste0("20", antr_m2$Jahr))
  antr_m2$Monat <- as.numeric(substr(antr_m2$Kurzname, start = 3, stop = 4))
  antr_m2$Halbjahr <- ifelse(antr_m2$Monat > 6, yes = "1. HJ", no = "2. HJ")
  antr_m2$Halbjahr <- paste(antr_m2$Jahr, antr_m2$Halbjahr, sep = "\n")
  #antr_m2$Halbjahr <- paste0(substr(start = 3, stop = 4, antr_m2$Jahr), "/", substr(antr_m2$Halbjahr, 1, 1))

  betei_m <- rbind(betei[, c("Name", "Anträge", "Disziplin", "Status")],
                   betei_arch[, c("Name", "Anträge", "Disziplin", "Status")])

  list(antr = antr_m2, betei = betei_m)
}
