options(timeout = 180)
fname <- "CA_full.csv.zip"
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
colnames(dat.full) <- c("iso.3166.code", "postal.code", "place", "province", "province.code",
"county", "county.code", "district", "district.code", "latitude", "longitude", "lat.long.acc")

keep.cols <- c("place", "postal.code", "province", "latitude", "longitude")
canada.postal.codes <- dat.full[, keep.cols]
canada.postal.codes[["province"]] <- as.factor(canada.postal.codes[["province"]])

qc.idx <- canada.postal.codes[["place"]] %in% "Quebec"
canada.postal.codes[qc.idx, "place"] <- "Quebec City"

regions <- c("Alberta" = "Prairies", "British Columbia" = "West",
             "Manitoba" = "Prairies", "New Brunswick" = "Atlantic",
             "Newfoundland and Labrador" = "Atlantic", "Northwest Territory" = "North",
             "Nova Scotia" = "Atlantic", "Nunavut Territory" = "North",
             "Ontario" = "Central", "Prince Edward Island" = "Atlantic",
             "Quebec" = "Central", "Saskatchewan" = "Prairies", "Yukon" = "North")
canada.postal.codes[["region"]] <- dplyr::recode(canada.postal.codes[["province"]],
                                                 !!!regions)
canada.postal.codes <- canada.postal.codes[, c("place", "postal.code", "region",
                                               "province", "latitude", "longitude")]
save(canada.postal.codes, file = "data/canada.postal.codes.rda", compress = TRUE)
