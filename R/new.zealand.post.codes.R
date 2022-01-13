#' Postcodes for New Zealand
#'
#' Postscodes, approximate latitude/longitude, region, local government area
#' for 1,738 places in New Zealand.
#'
#' \itemize{
#' \item place - character string place name
#' \item post.code - integer with 3-4 digits providing the full postcode for \code{place}
#' \item region - Factor with 16 levels; region of New Zealand.
#' \item lga - Factor with 132 levels; local government adminstrative area for place;
#' e.g. ward, local board, district or regional council, etc.
#' \item latitude - Numeric, approximate latitude for place
#' \item longitude - Numeric, approximate longitude for place
#' \item duplicate.place - Logical, indicating if \code{place} appears in more
#' than one region.
#' }
#' @name new.zealand.post.codes
#' @format A data.frame with 1,738 rows and 6 columns.
#' @usage data(new.zealand.post.codes)
#' @docType data
#' @keywords datasets
#' @source \url{https://download.geonames.org/export/zip/}
"new.zealand.post.codes"
