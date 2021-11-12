#' ISO-3166 2-letter country codes
#'
#' Two-letter abbreviate country names in ISO-3166 format with
#' additional information about availability of post code and synonym
#' information in other data sets.
#'
#' \itemize{
#' \item country.name - character string country name
#' \item country.code - two-character country code for the country in ISO-3166 format.
#' \item in.europe - logical indicating whether the country is located in Europe.
#' \item post.codes.available - logical indicating whether post code information is available for the country in one of the post code data files in the package.
#' \item synonyms.available - logical indicating whether synonym place names are available in \code{\link{synonyms}}
#' }
#' @name country.codes
#' @format A data.frame with 249 rows and 4 columns.
#' @usage data(country.codes)
#' @docType data
#' @keywords datasets
#' @source \url{https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes}
"country.codes"
