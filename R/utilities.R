data.list <- c(USA = "us.zip.codes",
               Canada = "canada.postal.codes",
               Europe = "euro.post.codes",
               UK = "uk.post.codes",
               Australia = "australia.post.codes",
               `New Zealand` = "new.zealand.post.codes")
available.types = list(USA = c("place", "zip.code", "region", "state", "county",
                               "latitude", "longitude"),
                       Canada = c("place", "postal.code", "region", "province",
                                  "latitude", "longitude"),
                       Europe = c("place", "post.code", "state", "province",
                                  "community", "country.code", "latitude", "longitude"),
                       UK = c("place", "zip.code", "country", "region", "county", "district",
                              "latitude", "longitude"),
                       Australia = c("place", "post.code", "state", "suburb",
                                     "lga", "region", "latitude", "longitude"),
                       `New Zealand` = c("place", "post.code", "region", "lga",
                                         "latitude", "longitude"))
utils::globalVariables(c("data.list", "available.types"))

#' Given a vector of text containing geographic
#' @noRd
detectRegion <- function(text, input.type = NULL, min.matches = 5)
{
    if (!is.null(input.type))
        input.type <- make.names(tolower(input.type))
    n <- length(text)
    min.matches <- min(n, min.matches)
    batch.size <- min(n, 50)
    possible.regions <- orderPossibleRegionsByRServer()
    region <- NULL
    idx <- seq_len(batch.size)
    while (is.null(region) && idx[length(idx)] <= n)
    {
        text.slice <- text[idx]
        for (curr.region in names(data.list))
        {
            if (is.null(input.type))
            {
                type <- detectInputType(text.slice, curr.region, min.matches)
                if (!is.na(type))
                {
                    region <- curr.region
                    break
                }
            }else
            {
                if (input.type %in% available.types[[curr.region]])
                {
                    matches <- findMatches(text.slice, curr.region, input.type,
                                           input.type, 0, FALSE)
                    if (sum(!is.na(matches)) >= min.matches)
                    {
                        region <- curr.region
                        break
                    }
                }
            }
        }
        max.idx <- idx[length(idx)]
        min.matches <- min(n, 2)  ## reduce tolerance if no matches on first pass
        idx <- (max.idx + 1):min(max.idx+batch.size, n)
    }
    if (is.null(region))
        stop("Unable to automatically determine region for the given input data. ",
             "Please specify the region and type of the input data and try again.")

    possible.int.postcodes <- grepl("^zip|^post", type) && region %in% c("USA",
                                                              "Australia", "Europe", "New Zealand")
    if (possible.int.postcodes) {
        region <- disambiguateIntegerPostcodes(text)
        type <- ifelse(region == "USA", "zip.code", "post.code")
    }
    if (is.null(input.type))
        attr(region, "input.type") <- type
    return(region)
}

disambiguateIntegerPostcodes <- function(text)
{
    MIN.MATCH.PROP <- .95
    txt.c <- text[!is.na(text)]
    n.char <- nchar(txt.c)
    txt.c <- txt.c[n.char > 0]
    if (any(n.char > 5) || any(grepl("[A-z -]", txt.c)))
        return("Europe")

    .checkPostCodes <- function(post.codes, region)
    {
        dat <- loadData(region)
        pc.name <- ifelse(region == "USA", "zip.code", "post.code")
        mean(post.codes %in% dat[[pc.name]], na.rm = TRUE)
    }

    if (mean(grepl("^[0-9]{3,4}$", txt.c)) >= MIN.MATCH.PROP)
    {  # could be any of Aus, NZ, Europe, USA; require ~all matching to declare region
        possible.regions <- c("Australia", "New Zealand", "USA", "Europe")
    }else  # text contains some five-digit numbers
        possible.regions <- c("USA", "Europe")

    pc.int <- suppressWarnings(as.integer(txt.c))
    match.props <- NULL
    for (r in possible.regions)
    {
        rprop <- .checkPostCodes(pc.int, r)
        if (rprop >= MIN.MATCH.PROP)
            return(r)
        match.props <- c(match.props, rprop)
    }
    return(possible.regions[which.min(match.props)])
}

detectInputType <- function(text, region, min.matches = 1)
{
    n <- length(text)
    min.matches <- min(n, min.matches)
    dat <- loadData(region)
    cols.to.check <- colnames(dat)
    keep.col <- !cols.to.check %in% c('latitude', 'longitude')
    cols.to.check <- cols.to.check[keep.col]
    ## check factors first since want to match e.g. state/province first
    col.classes <- vapply(dat, class, "")[keep.col]
    cols.to.check <- cols.to.check[order(col.classes, decreasing = TRUE)]

    input.type <- NA
    for(col in cols.to.check)
    {
        tbl <- dat[, col]
        if (is.factor(tbl))
            tbl <- levels(tbl)
        if (sum(text %in% tbl) >= min.matches)
        {
            input.type <- col
            break
        }
    }
    return(input.type)
}

#' Guess an appropriate output type given a region
#' and input.type
#'
#' Default to outputing places for postcode inputs and
#' vice versa.
#' @return string; output.type providing the column name
#' of the data.frame specified by code{region} to return
#' @noRd
deduceOutputType <- function(input.type, region)
{
    candidates <- available.types[[region]]
    if (input.type == "place")
        return(grep("^zip|^post", candidates, value = TRUE))
    if (grepl("^zip|^post", input.type))
        return("place")

    if (input.type == "state" && region == "Europe")
        return("country.code")

    ## columns of data frames are always arranged from
    ## largest subregion type to smallest, so pick the previous
    ## column from the input column position, to e.g. select
    ## output state for county input.type
    input.idx <- grep(input.type, candidates, fixed = TRUE)
    return(candidates[input.idx-1])
}

#' @importFrom data.table chmatch
#' @noRd
findMatches <- function(text, region, input.type, output.type, max.dist = 2,
                        check.types = FALSE, text.extra = NULL, ...)
{
    dat <- loadData(region)
    if (check.types)
    {
        input.type <- convertTypeForRegionIfAvailable(input.type, dat)
        output.type <- convertTypeForRegionIfAvailable(output.type, dat)
    }

    tbl <- dat[, input.type]
    if (grepl("(zip|post)\\.code", input.type) &&
        region %in% c("USA", "Australia", "New Zealand"))
    {
        text <- suppressWarnings(as.integer(text))
        found.idx <- match(text, tbl, incomparables = NA)
    }else
    {
        found.idx <- chmatch(text, as.character(tbl))
        if (anyNA(found.idx) && max.dist > 0)
        {
            na.idx <- which(is.na(found.idx))
            found.idx[na.idx] <- findNearMatches(text[na.idx], tbl, max.dist, ...)
        }
        if (input.type == "place" && !is.null(text.extra))
            found.idx <- disambiguatePlaceInputs(found.idx, text, text.extra,
                                                 dat, region, max.dist)
    }

    found <- as.character(dat[found.idx, output.type])
    return(found)
}

disambiguatePlaceInputs <- function(match.idx, text, disambig.text, dat, region, max.dist)
{
    ## check if found indexes are possible duplicates
    if (length(text) != length(disambig.text))
        stop("The length of ", sQuote("text.extra"), " must match the length of ",
             sQuote("text"), "(", length(text), ").")
    ## determine input type for disambig.text
    found.place <- !is.na(match.idx)
    disambig.text <- as.character(disambig.text)
    to.search.idx <- found.place & !is.na(disambig.text) & nzchar(disambig.text)
    if (any(to.search.idx))
    {
        txt <- paste0(text[to.search.idx], disambig.text[to.search.idx])
        disambig.type <- detectInputType(disambig.text, region, min.matches = 1)
        tbl <- paste0(dat[["place"]], dat[[disambig.type]])
        if (max.dist == 0)
            new.match.idx <- chmatch(txt, tbl)
        else
            new.match.idx <- findNearMatches(txt, tbl, max.dist)
        found.both <- !is.na(new.match.idx)
        match.idx[to.search.idx[found.both]] <- new.match.idx[found.both]
    }
    ## for each potential duplicate, append disambig.text and find match
    return(match.idx)
}

convertTypeForRegionIfAvailable <- function(type, dat, must.work = TRUE)
{
    if (grepl("^Place", type, ignore.case = TRUE))
        return("place")
    if (grepl("lga", type, ignore.case = TRUE))
        type <- "lga"
    else
        type <- make.names(tolower(type))
    TYPES <- c("place|city|town",
               "post.code|zip.code|postcode|postal.code",
               "state",
               "province",
               "county",
               "suburb|district|community",
               "lga",
               "region",
               "country|country.code")
    patt <- grep(type, TYPES, ignore.case = TRUE, value = TRUE)
    if (!length(patt))
    {
        if (!must.work)
            return(NA)
        valid.types <- paste0(gsub("\\|", "/", TYPES), collapse = ", ")
        stop("The requested type is not valid. ", sQuote("input.type"), " and ",
             sQuote("output.type"), " can be any of: ", valid.types, ".", call. = FALSE)
    }
    col.names <- colnames(dat)
    col.name <- grep(patt, col.names, value = TRUE)
    if (!length(col.name))
    {
        if (!must.work)
            return(NA)
        available.names <- paste0(col.names[!col.names %in% c('latitude',
                                                       'longitude', "country.code")],
                                  collapse = ", ")
        stop("The requested type for this region must be one of: ", available.names,
             ".", call. = TRUE)

    }
    return(col.name)
}

#' @importFrom stringdist amatch
#' @noRd
findNearMatches <- function(txt, tbl, max.dist, ...)
{
    return(amatch(txt, tbl, matchNA = FALSE, maxDist = max.dist,
                  method = "osa", ...))
}

#' If a region is not specified, package data is checked in
#' the order USA, Canada, Europe, UK, Australia
#' @noRd
orderPossibleRegionsByRServer <- function()
{
    possible.regions  <- data.list
    node <- Sys.info()[["nodename"]]
    if (grepl("^reausprod", node))
    {
        idx <- names(data.list) %in% c("Australia", "New Zealand")
        possible.regions <- possible.regions[c(which(idx), which(!idx))]
    }else if (grepl("^rweurprod", node))
    {
        idx <- names(data.list) %in% c("Australia", "New Zealand")
        possible.regions <- possible.regions[c(which(idx), which(!idx))]
    }
    return(possible.regions)
}

#' Used by Create New Variables - Transform Geography QScript
#' @noRd
determineGUIControlInput <- function(text, min.matches = 5)
{
    text <- convertToTitleCaseIfNecessary(text)
    region <- detectRegion(text, NULL, min.matches = min.matches)
    input.type <- attr(region, "input.type")
    output.type <- deduceOutputType(input.type, region)
    ## Capitalize first letter for dropbox values, convert zip to ZIP, lga to LGA
    .formatType <- function(type)
    {
        if (type == "post.code")
            return("Postcode")
        type <- sub("^([A-z](?:ip|ga)?)", "\\U\\1", type, perl = TRUE)
        type <- sub("\\.", " ", type, perl = TRUE)
        return(type)
    }
    input.type <- .formatType(input.type)
    output.type <- .formatType(output.type)
    return(c(region, input.type, output.type))
}

## findSynonyms <- function(text, region, type)
## {
##     mapped.synonyms <- rep(NA_character_, length(text))
##     if (admin1SynonymsRequested(region, type))
##         mapped.synonyms <- findAdmin1Synonyms(text, region)
##     else if (region == "Europe" && type %in% c("place", "province", "community"))
##         mapped.synonyms <- findSynonymsInTable(text)
##     return(mapped.synonyms)
## }

## admin1SynonymsRequested <- function(region, type)
## {
##     out <- FALSE
##     if (type == "state" || (region == "UK" && type == "county") ||
##         (region == "New Zealand" && type == "region") ||
##         (region == "Canada" && type == "province"))
##         out <- TRUE
##     return(out)
## }

## findRegionSynonyms <- function(text, region)
## {
##     ccodes <- regionToCountryCode(region)
##     data(admin1.synonyms, package = "flipGeoData")
##     idx <- admin1.synonyms[["country.code"]] %in% ccodes
##     admin1 <- admin1.synonyms[idx, ]
##     patts <- paste(admin1[, c("name", "name_en", "woe_name",
##                               "name_alt")], sep = "|")
##     found.synonyms <- vapply(patts, function(patt) grep(patt, text, useBytes = TRUE)[1L],
##                              1L, USE.NAMES = FALSE)


##     state.synonyms <- admin1.name.map[[region]]
##     synonyms.flat <- unlist(state.synonyms)
##     main.names <- sub("[0-9]+$", "", names(synonyms.flat))
##     found <- vapply(text, function(patt) grep(patt, synonyms.flat, fixed = TRUE)[1L],
##                     0L, USE.NAMES = FALSE)
##     if (all(is.na(found)))
##         return(found)
##     return(main.names[found])
## }

regionToCountryCode <- function(region)
{
    if (region == "Europe")
    {
        env <- new.env()
        data(country.codes, package = "flipGeoData", envir = env)
        country.codes <- get("country.codes", envir = env)
        idx <- country.codes[["in.europe"]] & country.codes[["post.codes.available"]]
        return(country.codes[idx, "country.code"])
    }else
    {
        return(switch(region,
                      USA = "US",
                      Canada = "CA",
                      Australia = "",
                      UK = "GB",
                      "New Zealand" = "NZ",
                      stop("Invalid region specified.", call. = FALSE)))
    }
}

#' Convert input text that is all upper or all lower case to title case
#' Only covert if text longer than 3 characters to avoid changing state abbreviations
#' @noRd
convertToTitleCaseIfNecessary <- function(txt)
{
    if (is.character(txt))
    {
        all.lower.or.upper <- grepl("^[[:upper:] .'-]{4,}$|^[[:lower:] .'-]{4,}$", txt)
        txt[all.lower.or.upper] <-  gsub("(\\w)(\\w*)", "\\U\\1\\L\\2", txt[all.lower.or.upper],
                                         perl=TRUE)
    }else if(is.factor(txt))
    {
        lvls <- levels(txt)
        all.lower.or.upper <- grepl("^[[:upper:] .'-]{4,}$|^[[:lower:] .'-]{4,}$", lvls)
        lvls[all.lower.or.upper] <- gsub("(\\w)(\\w*)", "\\U\\1\\L\\2", lvls[all.lower.or.upper],
                                         perl=TRUE)
        levels(txt) <- lvls
    }
    return(txt)
}

findMatchesInNeighbouringRegion <- function(text, region, input.type,
                                            output.type, max.dist, ...)
{
    neighbor <- switch(region, USA = "Canada", Canada = "USA",
                       Europe = "UK", UK = "Europe", Australia = "New Zealand",
                       "New Zealand" = "Australia")
    TYPE.MAP <- list("USA|Canada" = c(place = "place", zip.code = "postal.code",
                                      region = "region", state = "province", county = NA),
                     "Europe|UK"= c(place = "place", post.code = "post.code",
                                    state = "country", province = "county",
                                    community = "district", country.code = "country"),
                     "Australia|New Zealand" = c(place = "place", post.code = "post.code",
                                                 state = "region", suburb = NA, lga = "lga",
                                                 region = "region"))
    nbhr.pair <- grep(neighbor, names(TYPE.MAP), fixed = TRUE, value = TRUE)
    nbhr.type.map <- TYPE.MAP[[nbhr.pair]]
    if (startsWith(nbhr.pair, neighbor))
    {
        nbhr.input.type <- names(nbhr.type.map)[match(input.type, nbhr.type.map)]
        nbhr.output.type <- names(nbhr.type.map)[match(output.type, nbhr.type.map)]
    }else  # endsWith(nbhr.pair, neighbor)
    {
        nbhr.input.type <- nbhr.type.map[input.type]
        nbhr.output.type <- nbhr.type.map[output.type]
        if (is.na(nbhr.input.type) || is.na(nbhr.output.type))
            return(rep(NA_character_, length(text)))
    }

    ## data(list = data.list[[neighbor]], package = "flipGeoData", envir = environment())
    return(findMatches(text, neighbor, nbhr.input.type, nbhr.output.type, max.dist, FALSE))
}
