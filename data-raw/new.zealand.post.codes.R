library(xml2)
library(httr)
library(FNN)
library(tidygeocoder)

fname <- "NZ.zip"
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
colnames(dat.full) <- c("iso.3166.code", "post.code", "place", "region", "region.code",
                        "suburb", "suburb.code", "district", "district.code",
                        "latitude", "longitude", "lat.long.acc")

keep.cols <- c("place", "post.code", "region", "latitude", "longitude")
new.zealand.post.codes <- dat.full[, keep.cols]

## Use reverse geocoding to get LGAs from Nominatim
## 1,223 coordinates takes ~20min
system.time(lgas <- reverse_geocode(new.zealand.post.codes,
                                    lat = "latitude", long = "longitude"))
## replace diacritic/macron on 'u' in Manawatu-Whanganui
address <- gsub("Manawat.", "Manawatu", lgas[["address"]])
## Assumes all addresses end with format '[lga], [state], [country]'
## or '[lga], [state], [postcode], [country]'
patt <- paste0("^(?:.*, )?",  # street1+2
               "([A-z() '-]*), ",  # LGA
               "[A-z() -]*, ",  # state
               "(?:[0-9]*, )?",  # optional post code
               "New Zealand / Aotearoa$")  # country
patt <- paste0(#"^(?:.*, )?",  # street1+2
               "([A-z() '-]*), ",  # LGA
               "[A-z() -]*, ",  # state
               "(?:[0-9]*, )?",  # optional post code
               "New Zealand / Aotearoa$")  # country
lga.part <- sub(patt,
                "\\1", address, perl = TRUE)
states <- unique(new.zealand.post.codes[["region"]])
states <- states[nzchar(states)]
states[states %in% "Manawatu-Wanganui"] <- "Manawatu-Whanganui"

lga.part <- vapply(strsplit(address, ", "), function(x) {
    if (grepl("^[0-9]*$", x[1]))
        x <- x[-1]
    if (length(x) <= 3)
        return(NA_character_)
    revx <- rev(x)
    zip.part <- revx[2]
    has.zip <- grepl("[0-9]", zip.part)
    state <- ifelse(has.zip, revx[3], revx[2])
    has.state <- state %in% states
    if (has.state && has.zip && length(x) == 4L)
        return(NA_character_)
    return(revx[2 + has.zip + has.state])
}, "")

## address's with form LGA, region, country were returned as NA above
## compare with unique LGAs, to add them back in
na.idx <- which(is.na(lga.part))
ulga <- unique(lga.part)
patt <- "(?:([A-z ]*), )?([A-z '-]*), (?:[0-9]{4}, )?New Zealand / Aotearoa$"
m <- regexec(patt, address[na.idx])
parts <- regmatches(address[na.idx], m)
districtOrRegionalCouncil <- function(region)
    ifelse(region %in% c("Bay of Plenty", "Otago", "Canterbury", "West Coast", "Southland"),
           paste(region, "Regional Council"),
           paste(region, "District Council"))
missing.lgas <- vapply(parts, function(x)  # if no match in unique LGAs, use state
    ifelse(length(x) == 2 || x[2] %in% ulga, x[2],
           districtOrRegionalCouncil(x[3])), "")
lga.part[na.idx] <- missing.lgas

## region info missing from some places, so get it from address
regions.all <- new.zealand.post.codes[, "region"]
missing.region <- which(regions.all == "")
patt <- "(?:[A-z ]*, )?([A-z '-]*), (?:[0-9]{4}, )?New Zealand / Aotearoa$"
m <- regexec(patt, address[missing.region])
regions.fixed <- vapply(regmatches(address[missing.region], m), `[`, "", 2)
regions.all[missing.region] <- regions.fixed
regions.all[regions.all %in% "Manawatu-Wanganui"] <- "Manawatu-Whanganui"
new.zealand.post.codes[["region"]] <- as.factor(regions.all)

new.zealand.post.codes[["lga"]] <- as.factor(lga.part)
new.zealand.post.codes <- new.zealand.post.codes[, c("place", "post.code", "region",
                                                     "lga", "latitude", "longitude")]

## Add column to indicate if should disambiguate place name by appending region
## when converting place names
## A place may have multiple entries in the data.frame if
## it has multiple postal codes, but it shouldn't be considered
## a duplicate if it only occurs in one region.
place <- new.zealand.post.codes[["place"]]
region <- new.zealand.post.codes[["region"]]
potential.dup <- logical(length(place))
for (p in unique(place))
{
    p.idx <- which(place == p)
    if (length(p.idx) == 1L || length(unique(region[p.idx])) == 1L)
        next
    potential.dup[p.idx] <- TRUE
}
new.zealand.post.codes$duplicate.place <- potential.dup

save(new.zealand.post.codes, file = "data/new.zealand.post.codes.rda", compress = TRUE)
