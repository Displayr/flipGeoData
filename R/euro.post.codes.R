#' Postcodes for regions of Europe.
#'
#' Postcodes, approximate latitude/longitude, and state/province info for cities and towns in 42 European countries.
#'
#' \itemize{
#' \item place - character string place name
#' \item post.code - character postcode for place
#' \item state - Factor with 831 levels; state/1st order administrative subdivision
#' \item province - Factor with 6939 levels; province/2nd order administrative subdivision
#' \item community - Character; community/3rd order administrative subdivision

#' \item country.code - Factor with 42 levels; two-character country code for the country in ISO-3166 format.
#' \item latitude - Numeric, approximate latitude for region/place
#' \item longitude - Numeric, approximate longitude for region/place
#' \item duplicate.place - Logical, indicating if \code{place} appears in more
#' than one state
#' }
#' @name euro.post.codes
#' @format A data.frame with 719,147 rows and 8 columns.
#' @usage data(euro.post.codes)
#' @docType data
#' @keywords datasets
#' @source \url{https://download.geonames.org/export/zip/}
"euro.post.codes"
