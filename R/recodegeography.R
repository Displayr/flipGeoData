#' Recode geographic data to another geographic type
#'
#' Maps text containing geographic information (such as place names or
#' postcodes) for a world region to another type (such as
#' state/province or region).
#' @param text Character vector containing geographic text to be
#'     converted.
#' @param text.extra Character vector of the same length as
#'     \code{text} that provides additional info when \code{text}
#'     could match multiple places in a region. Ignored if
#'     \code{input.type} is not \code{"Place"} or auto-detected as
#'     such. E.g. \code{text.extra} could contain state information to
#'     disambiguate place names in \code{text} that occur in multiple states.
#' @param region String; providing the world region that the data in
#'     \code{text} comes from. Currently supported values are
#'     \code{"USA"}, \code{"Europe"}, \code{"Canada"}, \code{"UK"},
#'     \code{"Australia"}, \code{"New Zealand"}, or \code{NULL}. If
#'     \code{NULL}, an attempt is made to deduce the region
#'     automatically from \code{text}.
#' @param input.type String; the input type of \code{text},
#'     e.g. \code{"Postcode"} or \code{"Place"}. See the package data
#'     sets, \code{data(package='flipGeoData')}, for a list of
#'     supported types for each region. If \code{NULL}, an attempt is
#'     made to deduce the region automatically from \code{text} and
#'     \code{region}.
#' @param output.type String; the input type of \code{text},
#'     e.g. \code{"Postcode"} or \code{"LGA"}. See the package data
#'     sets, \code{data(package='flipGeoData')}, for a list of
#'     supported types for each region. Can be \code{NULL}, in which
#'     case it will be set based on the \code{input.type}.
#' @param check.neighboring.region Logical; if \code{TRUE}, then the
#'     nearest neighbouring region to \code{region} will also be
#'     checked for matches; i.e. \code{"Canada"} will be checked for
#'     matches in \code{text} when \code{region} is code{"USA"},
#'     \code{"New Zealand"} will also be checked when \code{region} is
#'     \code{"Australia"}, and \code{"UK"} will be checked when region
#'     is code{"Europe"}, and vice versa.
#' @param max.levenshtein.dist Integer; controlling approximate string
#'     matching; the maximum levenshtein distance allowed for input
#'     text to be considered a match for a value in the region's data
#'     set; see \code{\link{amatch}}. The default, \code{0}, means
#'     matches must be exact.
#' @param min.matches Integer; when automatically detecting
#'     \code{region} or \code{input.type} the minimum matches that
#'     must be present in \code{text} for a match for
#'     region/input.type to be considered found; default \code{5}.
#' @param ... Currently ignored.
#' @seealso \link{us.zip.codes}, \link{canada.postal.codes},
#'     \link{uk.post.codes}, \link{euro.post.codes},
#'     \link{australia.post.codes}, \link{new.zealand.post.codes}.
#' @export
#' @examples
#' RecodeGeography(501, region = "USA", input.type = "ZIP code", output.type = "Place")
#' RecodeGeography(c("Manitoba", "Quebec"))
RecodeGeography <- function(text,
                            text.extra = NULL,
                           region = NULL,
                           input.type = NULL,
                           output.type = NULL,
                           check.neighboring.region = FALSE,
                           max.levenshtein.dist = 0,
                           min.matches = 5,
                           ...)
{
    text <- convertToTitleCaseIfNecessary(text)
    if (!is.null(text.extra))
    {
        text.extra <- convertToTitleCaseIfNecessary(text.extra)
        if (is.factor(text))
            text.extra <- as.character(text.extra)
    }
    if (is.factor(text))
        text <- as.character(text)
    if (is.numeric(text))
    {
        if (!is.null(input.type) && !grepl("^(zip|post(al)?)[ .]?code$", input.type,
                                           ignore.case = TRUE))
            stop("The input data is numeric, but the ", sQuote("input.type"),
                 " parameter is ", dQuote(input.type), ". Only postcodes can be",
                 " numeric.")
    }
    if (is.null(region))
    {
        region <- detectRegion(text, input.type, min.matches)
        if (is.null(input.type))
            input.type <- attr(region, "input.type")
    }
    if (is.null(input.type))
        input.type <- detectInputType(text, region, min.matches)

    dat <- loadData(region)
    input.type <- convertTypeForRegionIfAvailable(input.type, dat)
    if (is.null(output.type))
        output.type <- deduceOutputType(input.type, region)


    output.type <- convertTypeForRegionIfAvailable(output.type, dat)
    ## check it is possible to convert input.type to output.type
    ## input.type must be smaller geographic unit than output.type
    errorIfInvalidMergeRequested(dat, input.type, output.type)

    if (admin1Type(input.type, region))
        text <- replaceAdmin1Synonyms(text, region, input.type)

    found <- findMatches(text, region, input.type, output.type, max.levenshtein.dist,
                         check.types = FALSE, text.extra,
                         error.if.ambiguous.place = TRUE, ...)

    if (check.neighboring.region && anyNA(found))
    {
        na.idx <- which(is.na(found))
        found.nhbr <- findMatchesInNeighbouringRegion(text[na.idx], region, input.type,
                          output.type, max.levenshtein.dist,
                          text.extra[na.idx], error.if.ambiguous.place = TRUE, ...)
        found[na.idx] <- found.nhbr
    }

    found <- as.character(found)
    found[is.na(found)] <- "Other"

    return(found)
}

#' @importFrom utils data
#' @noRd
loadData <- function(region)
{
    env <- new.env()
    data.set.name <- data.list[[region]]
    data(list = data.set.name, package = "flipGeoData", envir = env)
    return(get(data.set.name, envir = env))
}

