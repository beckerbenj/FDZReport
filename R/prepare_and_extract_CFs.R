#' Prepare and extract data of campus files data requests.
#'
#' Prepare and extract data of campus files data requests.
#'
#'@param cfs_uebersicht A \code{.xlsm} file including all data requests for campus files.
#'
#'@return A plot.
#'
#'@examples
#' # tbd
#'
#'@export
prepare_and_extract_cfs <- function(cfs_uebersicht) {
  cfs_ori <- readxl::read_xlsx(cfs_uebersicht, sheet = "CF-applications",
                              col_types = c("text", "text", "date", "date", "date", "text", "date", "text", "date",
                                            "text", "date", "date", "numeric", "text"))
  cfs <- cfs_ori[is.na(cfs_ori$`abgelehnt am`), c("Antragsdatum", "Studien")]

  #table(studien_ori$Status)
  cfs$Jahr <- as.numeric(substr(cfs$Antragsdatum, 1, 4))

  stopifnot(all(!is.na(cfs$Jahr)))
  cfs
}
