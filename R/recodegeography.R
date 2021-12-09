## Addresses work
## arg for synonyms
## warn if unmatched
## mix CDN/US
## mix EU/UK
## mix AU/NZ
## autodetect region
## autodetect from
## countries/states to abbreviations and vice versa
## function to detect zip codes
## factor inputs
## error if 'to' arg invalid for 'from'
## detect postcodes fun
## numeric post codes
## function mapping states to regions or state abbreviations
## region = "World" checks all data sets?
## multiple input types?
## a single misspelled input
## speed of NZ region autodetect (when not on reausprod); try gcinfo(TRUE)
#' @export
RecodeGeography <- function(text,
                           region = NULL,
                           input.type = NULL,
                           output.type = NULL,
                           check.synonyms = FALSE,
                           max.levenshtein.dist = 0,
                           min.matches = 5,
                           ...)
{
    text <- convertToTitleCaseIfNecessary(text)
    if (is.factor(text))
        text <- as.character(text)
    if (is.numeric(text))
    {
        if (!is.null(input.type) && input.type != "Postcode")
            stop("The input data is numeric, but the ", sQuote("input.type"),
                 " parameter is ", dQuote(input.type), ". Only post codes can be",
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
    if (FALSE)  ## check it is possible to convert input.type to output.type
        errorIfInvalidTypes(input.type, output.type)
    output.type <- convertTypeForRegionIfAvailable(output.type, dat)

    found <- findMatches(text, region, input.type, output.type, max.levenshtein.dist,
                       check.types = FALSE, ...)
    if (check.synonyms && anyNA(found))
    {
        na.idx <- which(is.na(found))
        mapped.synonyms <- findSynonyms(text[na.idx], region, input.type)
        found.syn <- findMatches(mapped.synonyms, region, input.type, output.type,
                                 max.dist = 0)
        found[na.idx] <- found.syn
    }
    if (FALSE && anyNA(found))
    {
        ## findMatchesInNeighbouringRegion(text, region, input.type,
        ##                                 output.type, max.levenshtein.dist, ...)
    }
    if (is.factor(found))  # R variables in Displayr get converted from factors to integer
        found <- as.character(found)
    return(found)
}

#' @importFrom utils data
#' @noRd
loadData <- function(region)
{
    env <- environment()
    data.set.name <- data.list[[region]]
    data(list = data.set.name, package = "flipGeoData", envir = env)
    return(get(data.set.name, envir = env))
}

