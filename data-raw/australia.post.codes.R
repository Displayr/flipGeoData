library(xml2)
library(httr)
library(FNN)
library(tidygeocoder)

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
missing.idx <- australia.post.codes[["suburb"]] == "NANA"
australia.post.codes[["suburb"]][missing.idx] <- NA
australia.post.codes[["state"]] <- as.factor(australia.post.codes[["state"]])


################################################################################
## Use Wikipedia to get LGA and region info
################################################################################
states <- levels(australia.post.codes[["state"]])
## Note: There are no LGAs for ACT
state_url_part <- states[!states %in% "Australian Capital Territory"]
state_url_part <- gsub(" ", "_", state_url_part)
nt.idx <- state_url_part %in% "Northern_Territory"
state_url_part[nt.idx] <- paste0("the_", state_url_part[nt.idx])
## Victoria's, Western Australia and South Australia's Wiki pages have multiple tables, one for
## Melbourne/Perth/Adelaide and one for each region
## other states have a single table


## Some tables have a column for Region, otherwise it must be read from
## preceding least-important heading
findRegions <- function(xml.table)
{
    ## some tables have region in 2nd col, some in 3rd
    headers <- xml_text(xml_find_all(xml.table, ".//th"))
    region.idx <- grep("^Region", headers)[1]
    if (!is.na(region.idx))
        return(trimws(xml_text(xml_find_all(xml.table,
                                            paste0(".//tr/td[", region.idx, "]")))))

    n.lgas <- length(xml_find_all(xml.table, ".//tr/td[1]"))
    ## Determine region from least important heading on page
    region <- xml_text(xml_find_first(xml.table, "preceding-sibling::h4[1]"))
    if (is.na(region))  # Only happens for Perth/Western Australia table
        region <- xml_text(xml_find_first(xml.table, "preceding-sibling::h3[1]"))
    if (is.na(region))
    {
        region <- xml_text(xml_find_first(xml.table, "preceding-sibling::h2[1]"))
        if (grepl("^Metropolitan LGAs", region)) {
            region <- "Perth"
        }else
            stop("Couldn't determine region")
    }

    region <- sub("\\[edit\\]$", "", region)
    return(rep(region, n.lgas))
}

lgas <- NULL
for (i in seq_along(state_url_part))
{
    state <- state_url_part[i]
    url <- paste0("https://en.wikipedia.org/wiki/",
                  ifelse(state == "New_South_Wales", "List_of_local_government_areas_in_",
                         "Local_government_areas_of_"), state)
    pl <- content(GET(url), encoding = "UTF-8")
    lga.tables <- xml_find_all(pl, "//table[@class='wikitable sortable']")
    lga.name <- trimws(xml_text(xml_find_all(lga.tables,
                                      ".//tr/td[1]")))
    region.name <- unlist(lapply(lga.tables, findRegions))
    ## replace unicode dash with standard hyphen
    lga.name <- gsub("\\p{Pd}", "-", lga.name, perl = TRUE)
    region.name <- gsub("\\p{Pd}", "-", region.name, perl = TRUE)
    lgas <- rbind(lgas, data.frame(state = states[i+1],  # +1 since No ACT
                                   region = region.name, lga = lga.name))
}

## Use geocoding to look up approximate lat/long for each region/LGA
patt <- paste0(",? ?", "(Regional Council|City Council|Communities Authority|",
               "Town Council|Community Council|District Council|Community Government Council|",
               "(?:Rural )?City|(?:Aboriginal )?Shire(?: Council)?|Region(?:al)?|Town",
               "|Community|Council|Corporation|Municipality)",
               "( of)? ?")
lga.lookup <- sub(patt, "", lgas[["lga"]], perl = TRUE)
## lga.lookup <- sub("^([A-z -]*), ([A-z ]* of)$", "\\2 \\1", lgas[["lga"]])
address <- paste(lga.lookup, lgas[["state"]], "Australia", sep = ", ")
## Fix a couple addresses geocoder doesn't like
address <- sub("^Hunter's Hill", "Hunters Hill", address)
address <- sub("^Mid-Western, New", "Mudgee, New", address)
address <- sub("^Unincorporated Far West, New", "Tibooburra, New", address)
lga.lat.long <- tidygeocoder::geo(address)

## For each place find nearest LGA, exclude islands (LGAs with no region) and
## two unicorporated areas of Darwin, NT
missing.coords <- is.na(lga.lat.long[["lat"]]) | lgas[["region"]] == ""
lat.long.no.islands <- lga.lat.long[!missing.coords, ]
nearest.coords <- FNN::get.knnx(data = lat.long.no.islands[, c("lat", "long")],
                             query = australia.post.codes[, c("latitude", "longitude")],
                             k = 1)
nearest.idx <- drop(nearest.coords$nn.index)
nearest.dist <- nearest.coords$nn.dist

################################################################################
## Geocoding doesn't do quite accurate enough job, so get LGAs by reverse geocoding
## LGAs are available in addresses returned by default API used by tidygeocoder
##
## For region info, still use Wikipedia tables

## Get LGA for every postcode
## WARNING: the following takes hours as it's pulling adddresses for 3313 post
## codes/2,871 (lat,long) pairs
first.post.code.idx <- !duplicated(australia.post.codes[["post.code"]])
pc.addresses <- reverse_geocode(australia.post.codes[first.post.code.idx, ],
                                lat = "latitude", long = "longitude")$address
## Assumes all addresses end with format '[lga], [state], [country]'
## or '[lga], [state], [postcode], [country]'
pc.lgas <- sub("^(?:.*, )?([A-z() '-]*), [A-z() -]*, ((?:NT )?[0-9]*, )?Australia$",
               "\\1", pc.addresses, perl = TRUE)
## Fix two edge cases, one from ACT another for an unicorporated island of Victoria
pc.lgas[pc.lgas %in% "Tidbinbilla Road"] <- "Australian Capital Territory"
unincorporated <- c("French Island", "Unincorporated Area Torrens Island",
                    "Pastoral Unincorporated Area")
pc.lgas[pc.lgas %in% unincorporated] <- "Unicorporated Area"
unique.lgas <- unique(pc.lgas)
post.codes <- australia.post.codes[["post.code"]]
unique.post.codes <- post.codes[first.post.code.idx]
names(pc.lgas) <- unique.post.codes
## dplyr::recode(post.codes, !!!pc.lgas)
lga.vec <- character(length(post.codes))
for (i in unique.post.codes) {
    lga.vec[post.codes %in% i] <- pc.lgas[as.character(i)]
}
australia.post.codes[["LGA"]] <- lga.vec


## (very) roughly, which places have been missclassified?
## worst10.idx <- order(nearest.dist, decreasing = TRUE)[1:10]
## cbind(lgas[["region"]][nearest.idx[worst10.idx]],
##       australia.post.codes[worst10.idx, "place"])
## head(sort(nearest.coords$nn.dist, decreasing = TRUE))
## DIST.CUTOFF <- 1
## potential.miss <- which(nearest.dist > DIST.CUTOFF)
## best.bad <- which.min(nearest.dist[potential.miss])
## ## verify Tablederry is correctly classified into Barcaldine Region LGA
## c(lgas[nearest.idx[potential.miss[best.bad]], ], australia.post.codes[bad.idx[potential.miss], ])
## ## Use reverse geocoding to get LGA for places with dist above cut-off
## ## Reverse geocoding every place in the data.frame would be prohibitively slow
## addresses <- reverse_geocode(australia.post.codes[potential.miss,],
##                              lat = "latitude", long = "longitude")$address
## ## missing.lgas <- vapply(strsplit(addresses, ", "), `[`, "", 2)
## ## All addresses end with format '[lga], [state], [country]' or '[lga], [state], [postcode], [country]'
## missing.lgas <- sub("^.*, ([A-z() -]*), [A-z() -]*, ([0-9]*, )?Australia$",
##                     "\\1", addresses)
## missing.lgas <- sub("Regional$", "Region", missing.lgas)
## missing.lgas <- sub("Council$", "", missing.lgas)
## ## LGAs from wikipedia have format 'Yalgoo, Shire of', 'Vincent, City of', etc.
## missing.lgas <- sub("([A-z]*) [Oo]f (.*)$", "\\2, \\1 of", missing.lgas)
## ## cbind(australia.post.codes[potential.miss, "place"], addresses)
## australia.post.codes$lga <- lgas[["lga"]][nearest.idx]
## act.idx <- australia.post.codes[["state"]] %in% "Australian Capital Territory"
## australia.post.codes$lga[act.idx] <- NA  # No LGA for ACT
## australia.post.codes[["lga"]][potential.miss] <- missing.lgas

## For region info, still use Wikipedia tables
australia.post.codes$region <- lgas[["region"]][nearest.idx]
missing.idx <- !nzchar(australia.post.codes[["region"]])
australia.post.codes[["region"]][missing.idx] <- NA
act.idx <- australia.post.codes[["state"]] %in% "Australian Capital Territory"
australia.post.codes[["region"]][act.idx] <- "Australian Capital Territory"
australia.post.codes[["region"]] <- as.factor(australia.post.codes[["region"]])
## australia.post.codes <- australia.post.codes[, c()]
save(australia.post.codes, file = "data/australia.post.codes.rda", compress = TRUE)
