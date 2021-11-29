#' Postal codes for Canada
#'
#' Postal codes, approximate latitude/longitude, and province for 892,800 places in Canada
#'
#' \itemize{
#' \item place - character string place name
#' \item postal.code - string with seven characters providing the full postal code for \code{place}
#' \item region - Factor with 5 levels; region of Canada: West, Prairies, North, Central,
#' or Atlantic.
#' \item province - Factor with 13 levels; province or territory/1st order administrative subdivision in Canada
#' \item latitude - Numeric, approximate latitude for region/place
#' \item longitude - Numeric, approximate longitude for region/place
#' }
#' @name canada.postal.codes
#' @format A data.frame with 892,800 rows and 6 columns.
#' @usage data(canada.postal.codes)
#' @docType data
#' @keywords datasets
#' @source \url{https://download.geonames.org/export/zip/}
"canada.postal.codes"
