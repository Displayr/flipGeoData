#' Postcodes for Australia.
#'
#' Postcodes, approximate latitude/longitude, state, suburb, region, and LGA info for cities and towns in Australia.
#'
#' \itemize{
#' \item place - character string place name
#' \item post.code - character post code for \code{place}
#' \item state - Factor with 8 levels; state/1st order administrative subdivision
#' \item suburb - Character; suburb/locality/community geographic subdivision; some entries are missing.
#' \item latitude - Numeric, approximate latitude for region/place
#' \item longitude - Numeric, approximate longitude for region/place
#' \item lga - Local government area; third level of government in Australia; e.g. shires,
#' cities, town, municipal council, etc.
#' \item region - Factor with 79 levels containing the geographic region within each state for
#' \code{place}. N/A for external territories of Australia .
#' }
#' @name australia.post.codes
#' @format A data.frame with 16,873 rows and 8 columns.
#' @usage data(euro.post.codes)
#' @docType data
#' @keywords datasets
#' @source \url{https://download.geonames.org/export/zip/} and \url{https://wikipedia.org}
"australia.post.codes"
