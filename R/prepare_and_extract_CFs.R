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
  ### someday: allow .csv import as this is the export format of the data base (via browser)

  # continue: seems to be a problem with "; in line 104?
  #cfs_ori <- read.csv2(cfs_uebersicht, row.names = NULL,
  #                     colClasses = c(NULL, NULL, "character", "character", "character", "character",
  #                                                 "character", "character", "character",
  #                                                "character", "character", "character", "character", "character"))
  #cfs <- cfs_ori[is.na(cfs_ori$`abgelehnt.am`) | cfs_ori$`abgelehnt.am` == "", c("Antragsdatum", "Studien")]
  #cfs <- cfs[cfs$Antragsdatum != "-", ]

  suppressWarnings(cfs_ori <- readxl::read_excel(cfs_uebersicht, sheet = 1,
                              col_types = c("text", "text", "date", "date", "date", "text",
                                           "date", "text", "date",
                                          "text", "date", "date", "numeric", "text")))
  cfs <- cfs_ori[is.na(cfs_ori$`abgelehnt am`), c("Antragsdatum", "Studien")]
  cfs <- cfs[!is.na(cfs$Antragsdatum), ]

  #table(cfs$Studien)
  cfs$Jahr <- as.numeric(substr(cfs$Antragsdatum, 1, 4))

  stopifnot(all(!is.na(cfs$Jahr)))
  cfs
}
