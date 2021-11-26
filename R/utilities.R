data.list <- c(USA = "us.zip.codes",
               Canada = "canada.postal.codes",
               Europe = "euro.post.codes",
               UK = "uk.post.codes",
               Australia = "australia.post.codes",
               `New Zealand` = "nz.post.codes")
available.types = list(USA = c("place", "zip.code", "state", "county",
                               "latitude", "longitude"),
                       Canada = c("place", "postal.code", "province",
                                  "latitude", "longitude"),
                       Europe = c("place", "zip.code", "state", "province",
                                  "community", "country.code", "latitude", "longitude"),
                       UK = c("place", "zip.code", "country", "county", "district",
                              "latitude", "longitude"),
                       Australia = c("place", "post.code", "state", "suburb",
                                     "latitude", "longitude", "LGA", "region"),
                       `New Zealand` = c("place", "post.code", "region",
                                         "latitude", "longitude"))
utils::globalVariables(c("data.list", "available.types"))


#' Given a vector of text containing geographic
detectRegion <- function(text, input.type)
{
    n <- length(text)
    min.matches <- min(n, 2)
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
                    print()
                    matches <- findMatches(text.slice, curr.region, input.type)
                    if (any(!is.na(matches)))
                    {
                        region <- curr.region
                        break
                    }
                }
            }
        }
        max.idx <- idx[length(idx)]
        min.matches <- 1  ## reduce tolerance if no matches on first pass
        idx <- (max.idx + 1):min(max.idx+batch.size, n)
    }
    if (is.null(region))
        stop("Unable to automatically determine region for the given input data. ",
             "Please specify the region and type of the input data and try again.")

    if (is.null(input.type))
        attr(region, "input.type") <- type
    return(region)
}

detectInputType <- function(text, region, min.matches = 1)
{
    dat <- loadData(region)
    cols.to.check <- colnames(dat)
    cols.to.check <- cols.to.check[!cols.to.check %in% c('latitude', 'longitude', "country.code")]
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

findMatches <- function(text, region, input.type, output.type, max.dist = 2,
                        check.types = FALSE, ...)
{
    dat <- loadData(region)
    if (check.types)
    {
        input.type <- convertTypeForRegionIfAvailable(input.type, dat)
        output.type <- convertTypeForRegionIfAvailable(output.type, dat)
    }

    tbl <- dat[, input.type]
    found.idx <- match(text, tbl)
    if (FALSE && anyNA(found.idx) && max.dist > 0)
    {
##        same.first.char <- startsWith(tbl, substring())
    }
    found <- rep(NA_character_, length(text))
    found <- dat[found.idx, output.type]
    return(found)
}

convertTypeForRegionIfAvailable <- function(type, dat)
{
    TYPES <- c("place|city|town",
               "post.code|zip.code|postcode|postal.code",
               "state|province",
               "suburb|district|community",
               "LGA",
               "region")
    patt <- grep(type, TYPES, ignore.case = TRUE, value = TRUE)
    if (!length(patt))
    {
        valid.types <- paste0(gsub("\\|", "/", TYPES), collapse = ", ")
        stop("The requested type is not valid. ", sQuote("input.type"), " and ",
             sQuote("output.type"), " can be any of: ", valid.types, ".", call. = FALSE)
    }
    col.names <- colnames(dat)
    col.name <- grep(patt, col.names, value = TRUE)
    if (!length(col.name))
    {
        available.names <- paste0(col.names[!col.names %in% c('latitude',
                                                       'longitude', "country.code")],
                                  collapse = ", ")
        stop("The requested type for this region must be one of: ", available.names,
             ".", call. = TRUE)

    }
    return(col.name)
}

#' If a region is not specified, package data is checked in
#' the order USA, Canada, Europe, UK, Australia
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

#' Used by Create New Variables - Recode Geography QScript
detemineGUIcontrolInput <- function(text)
{
    region <- detectRegion(text, NULL)
    input.type <- attr(region, "input.type")
    output.type <- deduceOutputType(input.type, region)
    return(c(region, input.type, output.type))
}
