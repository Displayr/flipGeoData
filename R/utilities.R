data.list <- c(USA = "us.zip.codes",
               Canada = "canada.postal.codes",
               Europe = "euro.post.codes",
               UK = "uk.post.codes",
               Australia = "australia.post.codes",
               `New Zealand` = "new.zealand.post.codes")
available.types = list(USA = c("zip.code", "place", "county", "state", "region"),
                       Canada = c("postal.code", "place", "province", "region"),
                       Europe = c("post.code", "place", "community", "province",
                                  "state", "country.code"),
                       UK = c("post.code", "place", "district", "county",
                              "region", "country"),
                       Australia = c("post.code", "place", "suburb", "lga",
                                     "region", "state"),
                       `New Zealand` = c("post.code", "place", "lga", "region"))
utils::globalVariables(c("data.list", "available.types"))

#' Given a vector of text containing geographic
#' @noRd
detectRegion <- function(text, input.type = NULL, min.matches = 5)
{
    if (!is.null(input.type))
    {
        input.type <- make.names(tolower(input.type))
        type <- input.type
    }
    n <- length(text)
    min.matches <- min(n, min.matches)
    batch.size <- min(n, 50)
    possible.regions <- orderPossibleRegionsByRServer()
    region <- NULL
    idx <- seq_len(batch.size)
    max.idx <- 1
    while (is.null(region) && max.idx < n)
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
    keep.col <- !cols.to.check %in% c("latitude", "longitude", "duplicate.place")
    cols.to.check <- cols.to.check[keep.col]
    ## check factors first since want to match e.g. state/province first
    col.classes <- vapply(dat, class, "")[keep.col]
    cols.to.check <- cols.to.check[order(col.classes, seq_along(col.classes),
                                         decreasing = TRUE)]

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
        return(switch(region, Canada = "province", "New Zealand" = "region",
                      UK = "region", "state"))
    if (grepl("^zip|^post", input.type))
        return("place")

    ## columns of data frames are always arranged from
    ## smallest subregion type to largest, so pick the next
    ## column from the input column position, to e.g. select
    ## output state for county input.type
    input.idx <- match(input.type, candidates)
    if (input.idx == length(available.types))
        stop("Sorry, it is not possible to merge data of input type ", sQuote(input.type),
             " for this region since there is no larger geographic unit available.")
    return(candidates[input.idx+1])
}

#' @importFrom data.table chmatch
#' @noRd
findMatches <- function(text, region, input.type, output.type, max.dist = 2,
                        check.types = FALSE, text.extra = NULL,
                        error.if.ambiguous.place = FALSE, ...)
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
    }else if (region == "Canada" && input.type == "postal.code")
    {
        found.idx <- matchPostalCodes(text, tbl)
    }else{
        found.idx <- chmatch(text, as.character(tbl))
        if (anyNA(found.idx) && max.dist > 0)
        {
            na.idx <- which(is.na(found.idx))
            found.idx[na.idx] <- findNearMatches(text[na.idx], tbl, max.dist, ...)
        }
        if (input.type == "place")
        {
            if (!is.null(text.extra))
                found.idx <- disambiguatePlaceInputs(found.idx, text, text.extra,
                                                     dat, region, max.dist)
            else if (error.if.ambiguous.place)
            {
                ambig.idx <- dat[found.idx, "duplicate.place"]
                if (any(ambig.idx, na.rm = TRUE))
                    ambiguousPlaceError(text[ambig.idx])
            }
        }
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
        ## found.both <- !is.na(new.match.idx)
        ## match.idx[which(to.search.idx)[found.both]] <- new.match.idx[found.both]
        match.idx[which(to.search.idx)] <- new.match.idx
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
                                                       'longitude', "country.code", "duplicate.place")],
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

admin1Type <- function(type, region)
{
    out <- FALSE
    if (type %in% c("state", "province") || (region == "UK" && type == "county") ||
        (region == "New Zealand" && type == "region"))
        out <- TRUE
    return(out)
}

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
                      Australia = "AU",
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
                                            output.type, max.dist, text.extra = NULL, error.if.ambiguous.place = FALSE, ...)
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
    return(findMatches(text, neighbor, nbhr.input.type, nbhr.output.type, max.dist, FALSE,
                       text.extra, error.if.ambiguous.place, ...))
}

#' @importFrom flipU IsRServer
#' @noRd
ambiguousPlaceError <- function (ambig.text)
{
    ambig.text <- unique(ambig.text[!is.na(ambig.text)])
    n.ambig <- length(ambig.text)
    ambig.places <- paste(ambig.text[1:min(n.ambig, 3)],
                          collapse = ", ")
    if (n.ambig > 3)
        ambig.places <- paste0(ambig.places, ", ...")
    instructions <- ifelse(flipU::IsRServer(),
                           paste0(sQuote("Extra Geographic Variable"), " dropdown."),
                           paste0(sQuote("text.extra"), " argument."))
    msg.start <- ngettext(n.ambig,
                     paste0("The following place cannot be unambiguously merged ",
                     "since it matches multiple locations in the region: "),
                     paste0("The following places cannot be unambiguously merged ",
                     "since they each match multiple locations in the region: "))
    stop(msg.start, ambig.places, ". Please supply an additional variable containing ",
         "state/province/region information to determine the proper ",
         "conversion via the ", instructions, call. = FALSE)
}

replaceAdmin1Synonyms <- function(text, region, input.type)
{
    idx <- admin1.synonyms[["region"]] == region & admin1.synonyms[["type"]] == input.type
    synonyms.for.region <- admin1.synonyms[idx, ]
    synonyms.tbl <- synonyms.for.region[["synonyms"]]
    synonyms.tbl <- strsplit(synonyms.tbl, "|", fixed = TRUE)
    names(synonyms.tbl) <- synonyms.for.region[["name"]]
    synonyms.tbl <- unlist(synonyms.tbl)

    found.idx <- chmatch(text, synonyms.tbl)
    out.txt <- sub("[0-9]*$", "", names(synonyms.tbl)[found.idx])
    no.synonym <- is.na(found.idx)
    out.txt[no.synonym] <- text[no.synonym]
    return(out.txt)
}

#' check it is possible to convert input.type to output.type
#' input.type must be smaller geographic unit than output.type
#' @noRd
errorIfInvalidMergeRequested <- function(dat, input.type, output.type)
{
    n.col <- ncol(dat)
    ## last three columns are always latitude, longitude, duplicate.place
    cnames <- colnames(dat)[1:(n.col-3)]
    in.pos <- match(input.type, cnames)
    out.pos <- match(output.type, cnames)
    ## columns of dat are always ordered from smallest geographic unit to largest
    if (in.pos == n.col - 3)
        stop("Sorry, it is not possible to merge data of input type ", sQuote(input.type),
             " for this region since there is no larger geographic unit available.")
    if (in.pos > out.pos)
        stop("It is not possible to merge from ", sQuote(input.type), " to ",
             sQuote(output.type), " for this region. Please specify an output type ",
             "that is a larger geographic unit than the input type.",
             call. = FALSE)
    return(invisible())
}

#' Match Canadian postal codes with or without middle space character
#' @noRd
matchPostalCodes <- function(txt, tbl)
{
    txt <- sub("^([A-z][0-9][A-z]) ?([0-9][A-z][0-9])$", "\\U\\1 \\U\\2", txt, perl = TRUE)
    return(chmatch(txt, tbl))
}
