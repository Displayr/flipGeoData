options(timeout = 180)
## tfile <- tempfile(fileext = ".zip")
## url <- "https://download.geonames.org/export/dump/allCountries.zip"
## download.file(url, tfile)
fname <- "GB_full.csv.zip"
if (!file.exists(fname))
{
    url <- paste0("https://download.geonames.org/export/zip/", fname)
    download.file(url, fname)
}
unzip(fname)

fname <- sub("csv\\.zip$", "txt", fname)
dat.full <- read.delim(file.path(dirname(fname), fname),
                       encoding = "UTF-8", header = FALSE)
## See https://download.geonames.org/export/zip/
colnames(dat.full) <- c("iso.3166.code", "post.code", "place", "country", "country.code",
"county", "county.code", "district", "district.code", "latitude", "longitude", "lat.long.acc")

## Add missing country/admin code1 names
idx <- which(dat.full[, "country.code"] == "L93000001")
dat.full[idx, "country"] <- "Channel Islands"
idx <- which(dat.full[, "country.code"] == "M83000003")
dat.full[idx, "country"] <- "Isle of Man"

## handle synonyms in Wales districts/communities, which are the
##  only districts that contain the separator text " - "
dat.full[["district"]] <- sub("^[A-z -]* - ", "", dat.full[["district"]])

keep.cols <- c("place", "post.code", "country", "county", "district",
               "latitude", "longitude")
uk.post.codes <- dat.full[, keep.cols]
uk.post.codes[["country"]] <- as.factor(uk.post.codes[["country"]])
uk.post.codes[["county"]] <- as.factor(uk.post.codes[["county"]])

## Get region info via reverse geocoding
library(tidygeocoder)
idx <- !duplicated(uk.post.codes[["county"]]) & uk.post.codes[["county"]] != ""
dat.ucounty <- uk.post.codes[idx, ]
dat.ucounty <- reverse_geocode(dat.ucounty, lat = "latitude", long = "longitude")


regions <- c("South East", "North West", "East of England", "West Midlands",
             "South West", "Yorkshire and the Humber", "East Midlands", "North East",
             "London", "Wales", "Northern Island", "Scotland")
## Make sure no address has multiple matches
patt <- paste0("(", paste(regions, collapse = ")|("), ")")
m <- regexec(patt, dat.ucounty[["address"]])
matches <- regmatches(dat.ucounty[["address"]], m)
fregions <- vapply(matches, function(vec){
    if (sum(nzchar(vec)) > 2)
        return(NA_character)
    return(vec[1])
}, "")

## special handling for one entry missing county and in England
missing.idx <- uk.post.codes[["county"]] == "" & uk.post.codes[["country"]] == "England"
dat.mcounty <- reverse_geocode(uk.post.codes[missing.idx, ], lat = "latitude", long = "longitude")
m <- regexec(patt, dat.mcounty[["address"]])
matches <- regmatches(dat.mcounty[["address"]], m)
mregions <- vapply(matches, `[`, "", 1)

anyNA(fregions)  # FALSE

names(fregions) <- dat.ucounty[["county"]]
county <- as.character(uk.post.codes[["county"]])
## All entries missing county aside from one above (Worthing) are islands, Wales, or NI
blank.county.idx <- county == ""
county[blank.county.idx] <- as.character(uk.post.codes[blank.county.idx, "country"])
country.vals <- levels(uk.post.codes[["country"]])
region.map <- c(fregions, setNames(country.vals, country.vals))
region <- dplyr::recode(county, !!!region.map)
region[missing.idx] <- mregions
uk.post.codes[["region"]] <- as.factor(region)

uk.post.codes <- uk.post.codes[, c("place", "post.code", "country", "region",
                                   "county", "district", "latitude", "longitude")]
save(uk.post.codes, file = "data/uk.post.codes.rda", compress = TRUE)
