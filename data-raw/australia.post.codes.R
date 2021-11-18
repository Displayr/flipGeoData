fname <- "AU.zip"
if (!file.exists(fname))
{
    url <- paste0("https://download.geonames.org/export/zip/", fname)
    download.file(url, fname)
}
unzip(fname)

fname <- sub("zip$", "txt", fname)
dat.full <- read.delim(file.path(dirname(fname), fname),
                       encoding = "UTF-8", header = FALSE)
## See https://download.geonames.org/export/zip/
colnames(dat.full) <- c("iso.3166.code", "post.code", "place", "state", "state.code",
                        "suburb", "suburb.code", "district", "district.code",
                        "latitude", "longitude", "lat.long.acc")

keep.cols <- c("place", "post.code", "state", "suburb", "latitude", "longitude")
australia.post.codes <- dat.full[, keep.cols]

simpleCap <- function(x) {
    if (!nzchar(x))
        return(NA_character_)
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)),
          sep = "", collapse = " ")
}
australia.post.codes[["suburb"]] <- vapply(australia.post.codes[["suburb"]],
                                           simpleCap, "", USE.NAMES = FALSE)
australia.post.codes[["state"]] <- as.factor(australia.post.codes[["state"]])

states <- levels(australia.post.codes[["state"]])

state_url_part <- gsub(" ", "_", states)[!states %in% "Australian Capital Territory"]

## Victoria's Wiki page has multiple tables, one for Melbourne and one for each region
## other states have a single table
## No LGAs for ACT
url <- paste0("https://en.wikipedia.org/wiki/",
              "Local_government_areas_of_Victoria")
pl <- content(GET(url))  # , encoding = "UTF-8")
lga.tables <- xml_find_all(pl, "//table[@class='wikitable sortable']")
melb.regions <- xml_text(xml_find_all(lga.tables[1], ".//tr/td[3]"))
other.regions <- xml_text(xml_find_all(pl, "//h3/span[@class='mw-headline']/a"))
n.lgas <- sapply(lga.tables[-1], function(tbl) length(xml_find_all(tbl, ".//tr/td[1]")))
lga.names <- xml_text(xml_find_all(lga.tables, ".//tr/td[1]"))
regions <- c(melb.regions, unlist(lapply(1:length(n.lgas), function(i) rep(other.regions[i],
                                                                           n.lgas[i]))))
lgas <- data.frame(state = "Victoria", region = regions, lga = lga.names)
## Add other states with a single table
state_url_part <- gsub(" ", "_", states)[!states %in% c("Australian Capital Territory", "Victoria")]
for (state in rev(state_url_part))
{
    url <- paste0("https://en.wikipedia.org/wiki/",
                  ifelse(state == "New_South_Wales", "List_of_local_government_areas_in_",
                         "Local_government_areas_of_"), state)
    pl <- content(GET(url), encoding = "UTF-8")
    lga.table <- xml_find_all(pl, "//table[@class='wikitable sortable']")
    if (state == "Western_Australia")
        lga.table <- lga.table[2]
    ## some tables have region in 2nd col, some in 3rd
    headers <- xml_text(xml_find_all(lga.table, ".//th"))
    region.idx <- which(headers == "Region")
    ## some rows of table only have a single column referencing
    ## another row of the table, need to filter these out
    ## country.names <- xml_text(xml_find_all(iso.code.table,
    ##                          ".//tr[count(td)>2]/td[1]/a[1]"))
    lga.name <- xml_text(xml_find_all(lga.table,
                                      ".//tr/td[1]/a[1]"))
    region.name <- xml_text(xml_find_all(lga.table,
                                         paste0(".//tr/td[", region.idx, "]/a[1]")))
    if (state == "Western_Australia")
    {   # extra table for perth
        lga.table <- xml_find_first(pl, "//table[@class='wikitable sortable']")
        perth.lga <- xml_text(xml_find_all(lga.table,
                                           ".//tr/td[1]/a[1]"))
        region.name <- c(rep("Perth", length(perth.lga)), region.name)
        lga.name <- c(perth.lga, lga.name)
    }
    ## replace unicode dash with standard hyphen
    lga.name <- gsub("\\p{Pd}", "-", lga.name, perl = TRUE)
    region.name <- gsub("\\p{Pd}", "-", region.name, perl = TRUE)
    lgas <- rbind(data.frame(state = gsub("_", " ", state), region = region.name,
                             lga = lga.name), lgas)
}

save(australia.zip.codes, file = "data/australia.zip.codes.rda", compress = TRUE)
