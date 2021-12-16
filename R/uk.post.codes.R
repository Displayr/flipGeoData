#' Postcodes for the United Kingdom
#'
#' Postcode unit, approximate latitude/longitude, county, and district/community info for countries in the United Kingdom.
#'
#' \itemize{
#' \item place - character string place name
#' \item post.code - string with 6-8 characters providing the full postcode unit
#' (incode and outcode) for \code{place}
#' \item country - Factor with six levels; country/1st order administrative subdivision
#' \item region - Factor with 14 levels; one of the nine regions of England or identical
#' to \code{country} if \code{place} is outside England.
#' \item county - Factor with 92 levels; county/2nd order administrative subdivision
#' \item district - Character; community/district/3rd order administrative subdivision
#' \item latitude - Numeric, approximate latitude for region/place
#' \item longitude - Numeric, approximate longitude for region/place
#' }
#' @name uk.post.codes
#' @format A data.frame with 1,791,546 rows and 7 columns.
#' @usage data(uk.post.codes)
#' @docType data
#' @keywords datasets
#' @source \url{https://download.geonames.org/export/zip/}
"uk.post.codes"
