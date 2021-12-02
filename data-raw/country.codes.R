library(httr)
library(xml2)

pl <- content(GET("https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes"), encoding = "UTF-8")
iso.code.table <- xml_find_first(pl, "//table[@class='wikitable sortable']")
## some rows of table only have a single column referencing
## another row of the table, need to filter these out
country.names <- xml_text(xml_find_all(iso.code.table,
                             ".//tr[count(td)>2]/td[1]/a[1]"))

## country.names <- substring(country.names, 2)
## country.names <- trimws(country.names)
alpha2.code <- xml_text(xml_find_all(iso.code.table,
                                     ".//td[4]/a"))
alpha2.code[1] <- sub("^.*([A-Z]{2})$", "\\1", alpha2.code[1])
names(alpha2.code) <- country.names


loadPkgData <- function(fname)
{
    ##    data.file <- system.file("data", "euro.post.codes.rdata",
    ##                             package = "flipGeoData")
    data.file <- file.path("../data/", fname)
    if (!file.exists(data.file))
    {
        file.loc <- paste0("data-raw/", sub("[.][A-z]*$", ".R", fname))
        stop("First run code in ", file.loc,
             " to create this data set.")

    }
    load(data.file, .GlobalEnv)
    return(invisible())
}

loadPkgData("euro.post.codes.rda")
loadPkgData("synonyms.rda")

## europe states only
url <- paste0("https://en.wikipedia.org/wiki/",
              "List_of_sovereign_states_and_dependent_territories_in_Europe")
pl <- content(GET(url))
euro.table <- xml_find_first(pl, "//table[@class='wikitable sortable']")
euro.countries <- xml_text(xml_find_all(euro.table, ".//td[3]/a[1]"))
euro.codes <- vapply(euro.countries, function(cname)
{
    idx <- grep(cname, country.names, fixed = TRUE)
    if (length(idx)){
        return(alpha2.code[idx[1]])
    }else {
        idx <- grep(cname, country.names, value = TRUE)
        return(ifelse(length(idx), alpha2.code[idx[1]], NA_character_))
    }
}, "")
euro.codes["Vatican City"] <- "VC"
euro.codes["Czech Republic"] <- "CZ"

country.names <- names(alpha2.code)
country.names <- sub(" [(][A-z .-]*[)]$", "", country.names)
country.names[country.names %in% "Viet Nam"] <- "Vietnam"

country.codes <- data.frame(country.name = names(alpha2.code),
                    country.code = unname(alpha2.code),
                    in.europe = alpha2.code %in% euro.codes)

available.post.codes <- c("US", "CA", "AU", "NZ",
                          levels(euro.post.codes[["country.code"]]))
available.synonyms <- unique(synonyms[["country.code"]])
country.codes[["post.codes.available"]] <- country.codes[["country.code"]] %in% available.post.codes
country.codes[["synonyms.available"]] <- country.codes[["country.code"]] %in% available.synonyms

tdir <- tempdir()
save(country.codes, file = file.path(tdir, "country.codes.rda"))

