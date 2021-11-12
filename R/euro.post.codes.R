#' Post codes for regions of Europe.
#'
#' Post codes, approximate latitude/longitude, and state/province info for cities and towns in 42 European countries.
#'
#' \itemize{
#' \item place - character string place name
#' \item zip.code - character zip code for place
#' \item state - Character; state/1st order administrative subdivision
#' \item province - Character; province/2nd order administrative subdivision
#' \item state - Character; community/3rd order administrative subdivision

#' \item country.code - Factor with 42 levels; two-character country code for the country in ISO-3166 format.
#' \item latitude - Numeric, approximate latitude for region/place
#' \item longitude - Numeric, approximate longitude for region/place
#' \item time.zone - Factor with 84 levels containing the time zone for the region/place.
#' }
#' @name euro.post.codes
#' @format A data.frame with 719,147 rows and 8 columns.
#' @usage data(euro.post.codes)
#' @docType data
#' @keywords datasets
#' @source \url{https://download.geonames.org/export/zip/}
"euro.post.codes"
