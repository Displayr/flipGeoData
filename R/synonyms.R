#' Alternative names and ASCII names for several world regions, towns, and cities.
#'
#' Alternative names and ASCII names for several world regions, towns, and cities.
#'
#' \itemize{
#' \item name - character string place name
#' \item ascii.name - ASCII character representation of \code{name}
#' \item alternate.names - Comma-separated string listing alternative names for the region/place
#' \item country.code - Factor with 18 levels; two-character country code for the country in ISO-3166 format.
#' \item latitude - Numeric, approximate latitude for region/place
#' \item longitude - Numeric, approximate longitude for region/place
#' \item time.zone - Factor with 84 levels containing the time zone for the region/place.
#' }
#' @name synonyms
#' @format A data.frame with 356,268 rows and 7 columns.
#' @usage data(synonyms)
#' @docType data
#' @keywords datasets
#' @source \url{https://download.geonames.org/export/dump/}
"synonyms"
