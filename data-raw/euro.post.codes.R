options(timeout = 180)
## tfile <- tempfile(fileext = ".zip")
## url <- "https://download.geonames.org/export/dump/allCountries.zip"
## download.file(url, tfile)
fname <- "allCountries.zip"
if (!file.exists(fname))
{
    url <- "https://download.geonames.org/export/zip/allCountries.zip"
    download.file(url, fname)
}

unzip(fname)
dat.full <- read.delim(file.path(dirname(fname), "allCountries.txt"),
                       encoding = "UTF-8", header = FALSE)

## See https://download.geonames.org/export/zip/
colnames(dat.full) <- c("country.code", "zip.code", "place", "state", "state.code",
"province", "province.code", "community", "community.code", "latitude", "longitude", "lat.long.acc")


keep.codes <- c("AL", "AD", "AM", "AT", "AZ", "BY", "BE", "BA", "BG",
                "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "GE", "DE",
                "GR", "HU", "IS", "IE", "IT", "KZ", "LV", "LI", "LT",
                "LU", "MT", "MD", "MC", "ME", "NL", "MK", "NO", "PL",
                "PT", "RO", "RU", "SM", "RS", "SK", "SI", "ES", "SE",
                "CH", "TR", "UA", "GB", "VC")

euro.post.codes <- dat.full[dat.full[["country.code"]] %in% keep.codes, ]
euro.post.codes[["country.code"]] <- as.factor(euro.post.codes[["country.code"]])

keep.cols <- c("place", "zip.code", "state", "province", "community",
               "country.code", "latitude", "longitude")
euro.post.codes <- euro.post.codes[, keep.cols]

save(euro.post.codes, file = "data/euro.post.codes.rda", compress = TRUE,
     compression_level = 9)
