fname <- "US.zip"
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
colnames(dat.full) <- c("iso.3166.code", "zip.code", "place", "state", "state.code",
                        "county", "county.code", "district", "district.code",
                        "latitude", "longitude", "lat.long.acc")

## Remove Marshall Islands and Armed Forces Zip Codes
us.zip.codes <- dat.full[!dat.full[["state"]] %in% c("", "Marshall Islands"), ]

## some counties contain extra text "(city)" or "(CA)" (census area)
us.zip.codes[["county"]] <- sub(" \\(city\\)?$", "", us.zip.codes[["county"]])
us.zip.codes[["county"]] <- sub("\\(CA?\\)?$", ## one CA ends with text "(C"
                                "Census Area", us.zip.codes[["county"]])
us.zip.codes[["state"]] <- as.factor(us.zip.codes[["state"]])

keep.cols <- c("place", "zip.code", "state", "county", "latitude", "longitude")
us.zip.codes <- us.zip.codes[, keep.cols]

save(us.zip.codes, file = "data/us.zip.codes.rda", compress = TRUE)
