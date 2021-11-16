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
colnames(dat.full) <- c("iso.3166.code", "zip.code", "place", "country", "country.code",
"county", "county.code", "district", "district.code", "latitude", "longitude", "lat.long.acc")

## Add missing country/admin code1 names
idx <- which(dat.full[, "country.code"] == "L93000001")
dat.full[idx, "country"] <- "Channel Islands"
idx <- which(dat.full[, "country.code"] == "M83000003")
dat.full[idx, "country"] <- "Isle of Man"

## handle synonyms in Wales districts/communities, which are the
##  only districts that contain the separator text " - "
dat.full[["district"]] <- sub("^[A-z -]* - ", "", dat.full[["district"]])

keep.cols <- c("place", "zip.code", "country", "county", "district",
               "latitude", "longitude")
uk.post.codes <- dat.full[, keep.cols]
uk.post.codes[["country"]] <- as.factor(uk.post.codes[["country"]])
uk.post.codes[["county"]] <- as.factor(uk.post.codes[["county"]])

save(uk.post.codes, file = "data/uk.post.codes.rda", compress = TRUE)
