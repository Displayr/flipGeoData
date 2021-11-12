options(timeout = 180)
fname <- "allCountriesGazetteer.zip"
if (!file.exists(fname))
{
    url <- "https://download.geonames.org/export/dump/allCountries.zip"
    download.file(url, fname)
}

unzip(fname)
dat.full <- read.delim(file.path(dirname(fname), "allCountries.txt"),
                       encoding = "UTF-8", header = FALSE)

## See https://download.geonames.org/export/dump/
colnames(dat.full) <- c("geonameid",         # integer id of record in geonames database
"name",              # name of geographical point (utf8) varchar(200)
"ascii.name",         # name of geographical point in plain ascii characters, varchar(200)
"alternate.names",    # alternatenames, comma separated, ascii names automatically transliterated, convenience attribute from alternatename table, varchar(10000)
"latitude",          # latitude in decimal degrees (wgs84)
"longitude",         # longitude in decimal degrees (wgs84)
"feature.class",     # see http://www.geonames.org/export/codes.html, char(1)
"feature.code",      # see http://www.geonames.org/export/codes.html, varchar(10)
"country.code",      # ISO-3166 2-letter country code, 2 characters
"cc2",               # alternate country codes, comma separated, ISO-3166 2-letter country code, 200 characters
"admin1.code",       # fipscode (subject to change to iso code), see exceptions below, see file admin1Codes.txt for display names of this code; varchar(20)
"admin2.code",       # code for the second administrative division, a county in the US, see file admin2Codes.txt; varchar(80)
"admin3.code",       # code for third level administrative division, varchar(20)
"admin4.code",      # code for fourth level administrative division, varchar(20)
"population",        # bigint (8 byte int)
"elevation",         # in meters, integer
"dem",               # digital elevation model
"time.zone",          # the iana timezone id (see file timeZone.txt) varchar(40)
"modification.date") # date of last modification in yyyy-MM-dd format

keep.codes <- c("AL", "AD", "AM", "AT", "AZ", "BY", "BE", "BA", "BG",
                "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "GE", "DE",
                "GR", "HU", "IS", "IE", "IT", "KZ", "LV", "LI", "LT",
                "LU", "MT", "MD", "MC", "ME", "NL", "MK", "NO", "PL",
                "PT", "RO", "RU", "SM", "RS", "SK", "SI", "ES", "SE",
                "CH", "TR", "UA", "GB", "VC", "US", "CA", "NZ", "AU")

## Keep only regions, towns, etc. http://www.geonames.org/export/codes.html
synonyms <- dat.full[grepl("^F|^P", dat.full[["feature.code"]]), ]

synonyms <- synonyms[synonyms[["country.code"]] %in% keep.codes, ]
synonyms[["country.code"]] <- as.factor(synonyms[["country.code"]])
synonyms[["time.zone"]] <- as.factor(synonyms[["time.zone"]])

keep.cols <- c("name", "ascii.name", "alternate.names", "country.code", "latitude", "longitude", "time.zone")
synonyms <- synonyms[, keep.cols]

save(synonyms, file = "data/synonyms.rda", compress = TRUE,
     compression_level = 9)
