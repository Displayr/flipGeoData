#' Zip codes for the United States of America
#'
#' Zip codes, approximate latitude/longitude, state, and county for 40,970 places in the United States.
#'
#' \itemize{
#' \item place - Character; place name
#' \item zip.code - 3-5 digit integer zip code for \code{place}
#' \item region - Factor with four levels; region of the U.S.: West, Midwest, South, or Northeast.
#' \item state - Factor with 51 levels; state/1st order administrative subdivision in the U.S.
#' \item county - Character; county or census area/2nd order administrative subdivision in the U.S.
#' \item latitude - Numeric, approximate latitude for place
#' \item longitude - Numeric, approximate longitude for place
#' }
#' @name us.zip.codes
#' @format A data.frame with 40,970 rows and 7 columns.
#' @usage data(us.zip.codes)
#' @docType data
#' @keywords datasets
#' @source \url{https://download.geonames.org/export/zip/}
"us.zip.codes"
