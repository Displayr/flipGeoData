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
                           max.levenstein.dist,
                           ...)
{
    if (is.factor(text))
        text <- as.character(text)
    if (is.numeric(text))
    {
        if (!is.null(input.type) || input.type != "Postcode")
            stop("The input data is numeric, but the ", sQuote("input.type"),
                 " parameter is ", dQuote(input.type), ". Only post codes can be",
                 " numeric.")
    }
    if (is.null(region))
    {
        region <- detectRegion(text, input.type)
        if (is.null(input.type))
            input.type <- attr(region, "input.type")
    }
    if (is.null(input.type))
        input.type <- detectInputType(text, region)

    if (is.null(output.type))
        output.type <- deduceOutputType(input.type, region)
    if (FALSE)  ## check it is possible to convert input.type to output.type
        errorIfInvalidTypes(input.type, output.type)
    dat <- loadData(region)
    input.type <- convertTypeForRegionIfAvailable(input.type, dat)
    output.type <- convertTypeForRegionIfAvailable(output.type, dat)

    found <- findMatches(text, region, input.type, output.type, max.levenstein.dist,
                       check.types = FALSE, ...)
    if (FALSE && check.synonyms && anyNA(found.idx))
    {
        na.idx <- which(is.na(found))

    }
    if (FALSE && anyNA(found.idx))
    {
        findMatchesInNeighbouringRegion(text, region, input.type,
                                        output.type, max.levenstein.dist, ...)
    }

    return(found)
}

loadData <- function(region)
{
    env <- environment()
    data.set.name <- data.list[[region]]
    data(list = data.set.name, package = "flipGeoData", envir = env)
    return(get(data.set.name, envir = env))
}

