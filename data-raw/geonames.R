## xpath cheatsheet: https://devhints.io/xpath#chaining-order
## rando xpaths: contains(text(),"Go")
################################################################################
## Germany
## Q?
## * Wikipedia says 401 districts, has table with 403 entries, 379 after removing
## duplicates listed twice as both Rural and Urban
## * GitHub resource lists


library(httr)
library(xml2)
url <- "https://en.wikipedia.org/wiki/List_of_cities_and_towns_in_Germany"
pl <- content(GET(url))

nodes <- xml_find_all(pl, './/div[@class!="navbox"]/table/tbody/tr/td')
ntext <- xml_text(nodes)
city.and.region <- unlist(strsplit(ntext, "\n"))
city.and.region <- city.and.region[nzchar(city.and.region)]
cities.no.synonyms <- regmatches(city.and.region, regexec("^([^(]*)", city.and.region))
cities.no.synonyms <- trimws(vapply(cities.no.synonyms, `[`, "", 2))

parts <- regmatches(city.and.region, regexec("^(.*) \\(([^(]*)\\)$", city.and.region))
cities <- vapply(parts, `[`, "", 2)  # includes synonyms in parentheses
regions <- vapply(parts, `[`, "", 3)

uri <- "en.wikipedia.org/wiki/"
for (i in seq_along(cities))
{
    pl <- content(GET(paste0(uri, cities[i])))
}

## list of post codes
## https://en.wikipedia.org/wiki/List_of_districts_of_Germany lists 401 districts
##  aka Kreis aka Landrkeis
## Below file from https://github.com/zauberware/postal-codes-json-xml-csv has 399 communities
de.districs <- content(GET("https://en.wikipedia.org/wiki/List_of_districts_of_Germany"))
district.table <- xml_find_first(de.districs, ".//table[@class='wikitable sortable']")
districts <- xml_text(xml_find_all(district.table, ".//td[1]"))
districts <- sub("\\n$", "", districts)
de.post.codes <- read.csv("https://github.com/zauberware/postal-codes-json-xml-csv/raw/master/data/DE/zipcodes.de.csv", encoding = "UTF-8")
length(unique(de.post.codes[["community"]]))

dim(de.post.codes)  # 16481 x 11

## discrepencies between Wikipedia and GitHub data:
missing.cities <- which(!cities.no.synonyms %in% de.post.codes[["place"]])
length(missing.cities)/length(cities.no.synonyms)
cities.no.synonyms[missing.cities]
sum(cities.no.synonyms[missing.cities] %in% de.post.codes[["community"]])  # 1

## does "community" in GitHub data match districts/kreis from GitHub?
missing.districts <- which(!districts %in% de.post.codes[["community"]])  # 224
length(missing.districts)/length(districts)
sum(districts[missing.districts] %in% de.post.codes[["place"]])  # 143
missing.from.both <- !districts %in% de.post.codes[["place"]] &
    !districts %in% de.post.codes[["community"]]
districts[missing.from.both]
head(districts[missing.districts], 20)

## for
length(grep("[Ll]andkreis", unique(de.post.codes[["community"]])))  # 141
length(grep("[Kk]reis", unique(de.post.codes[["community"]])))  # 225


################################################################################
## UK
## postcodesioR uses postcodes.io

################################################################################
## geonames.org - Postal codes for 100 countries
## "longitude")
## col1: country code
## col2: post code
## col3: place
## col4: state
## col5: state_code
## col6: province
## col7: province_code
## col8: community
## col9: community code
## col10: lattitude
## col11: longitude
## col12: integer; accuracy of lat/long from 1-6
## cols 1-11 match German GH data
## The data format is tab-delimited text in utf8 encoding, with the following fields :
## country code      : iso country code, 2 characters
## postal code       : varchar(20)
## place name        : varchar(180)
## admin name1       : 1. order subdivision (state) varchar(100)
## admin code1       : 1. order subdivision (state) varchar(20)
## admin name2       : 2. order subdivision (county/province) varchar(100)
## admin code2       : 2. order subdivision (county/province) varchar(20)
## admin name3       : 3. order subdivision (community) varchar(100)
## admin code3       : 3. order subdivision (community) varchar(20)
## latitude          : estimated latitude (wgs84)
## longitude         : estimated longitude (wgs84)
## accuracy          : accuracy of lat/lng from 1=estimated, 4=geonameid, 6=centroid of addresses or shape

tfile <- tempfile(fileext = ".zip")
url <- "http://download.geonames.org/export/zip/allCountries.zip"
download.file(url, tfile)
unzip(tfile, exdir = dirname(tfile))
geo.names.db <- read.delim(file.path(dirname(tfile), "allCountries.txt"),
                           encoding = "UTF-8", header = FALSE)
colnames(geo.names.db) <- c("country_code", "zipcode", "place", "state", "state_code",
"province", "province_code", "community", "community_code", "latitude", "longitude")
## geo.names.db <- read.delim("~/Documents/Features/GeoCategories/allCountries/allCountries.txt",
##                            encoding = "UTF-8")
country.codes.gndb <- unique(geo.names.db[, 1])

################################################################################
## ISO-3601 country codes
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
## alpha2.code <- trimws(alpha2.code[-1])

## europe states only
pl <- content(GET("https://en.wikipedia.org/wiki/List_of_sovereign_states_and_dependent_territories_in_Europe"))
euro.table <- xml_find_first(pl, "//table[@class='wikitable sortable']")
euro.countries <- xml_text(xml_find_all(euro.table, ".//td[3]/a"))
euro.countries <- euro.countries[!euro.countries %in% "Kingdom of the Netherlands"]
euro.codes <- vapply(euro.countries, function(cname){
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
anyNA(euro.codes)

## euro countries missing from geonames.org
## 8 of 50: Albania, Armenia, Bosnia and Herzegovina, Georgia,
## Greece, Kazakhstan, Montenegro, Vatican City
euro.codes[!euro.codes %in% alpha2.code]


## filter geonames db for only euro countries
euro.db <- geo.names.db[geo.names.db[, 1] %in% euro.codes, -12]
colnames(euro.db) <- colnames(de.post.codes)
## colnames(euro.db) <- c("country_code", "zipcode", "place", "state", "state_code",
## "province", "province_code", "community", "community_code", "latitude", "longitude")

## compare geonames db with DE post codes from GH
de.db <- euro.db[euro.db[, 1] %in% "DE", ]
dim(de.db)
dim(de.post.codes)
matrix(c(de.db[1, 1:11], de.post.codes[1, ]), nrow = 2,
       byrow = TRUE, dimnames = list(NULL, colnames(de.post.codes)))

## UK - allCountries.zip from geocodes.org only has outcodes
uk.db <- euro.db[euro.db[, 1] %in% "GB", ]
unique(uk.db[, "state_code"])

tfile <- tempfile(fileext = ".zip")
url <- "http://download.geonames.org/export/zip/GB_full.csv.zip"
download.file(url, tfile)
unzip(tfile, exdir = dirname(tfile))
gb.db <- read.delim(file.path(dirname(tfile), "GB_full.txt"),
                           encoding = "UTF-8", header = FALSE)
colnames(geo.codes.db)

################################################################################
## Canada
tfile <- tempfile(fileext = ".zip")
url <- "http://download.geonames.org/export/zip/CA_full.csv.zip"
download.file(url, tfile)
unzip(tfile, exdir = dirname(tfile))
ca.db <- read.delim(file.path(dirname(tfile), "CA_full.txt"),
                           encoding = "UTF-8", header = FALSE)
colnames(ca.db) <- c("country_code", "zipcode", "place", "state", "state_code",
"province", "province_code", "community", "community_code", "latitude", "longitude", "geo_acc")

## US
uk.db <- euro.db[euro.db[, 1] %in% "GB", ]

tfile <- tempfile(fileext = ".zip")
url <- "https://download.geonames.org/export/dump/alternateNamesV2.zip"
download.file(url, tfile)
unzip(tfile, exdir = dirname(tfile))
alt.names <- read.delim(file.path(dirname(tfile), "alternateNamesV2.txt"),
                                 encoding = "UTF-8", header = FALSE)

################################################################################
## geonames Gazetteer data

options(timeout = 180)​
## tfile <- tempfile(fileext = ".zip")
## url <- "https://download.geonames.org/export/dump/allCountries.zip"
## download.file(url, tfile)
tfile <- "~/Documents/Features/GeoCategories/allCountries.zip"
unzip(tfile, exdir = dirname(tfile))
dat.full <- read.delim(file.path(dirname(tfile), "allCountries.txt"),
                                encoding = "UTF-8", header = FALSE)
colnames(dat.full) <- c("geonameid",         # integer id of record in geonames database
"name",              # name of geographical point (utf8) varchar(200)
"asciiname",         # name of geographical point in plain ascii characters, varchar(200)
"alternatenames",    # alternatenames, comma separated, ascii names automatically transliterated, convenience attribute from alternatename table, varchar(10000)
"latitude",          # latitude in decimal degrees (wgs84)
"longitude",         # longitude in decimal degrees (wgs84)
"feature class",     # see http://www.geonames.org/export/codes.html, char(1)
"feature code",      # see http://www.geonames.org/export/codes.html, varchar(10)
"country code",      # ISO-3166 2-letter country code, 2 characters
"cc2",               # alternate country codes, comma separated, ISO-3166 2-letter country code, 200 characters
"admin1 code",       # fipscode (subject to change to iso code), see exceptions below, see file admin1Codes.txt for display names of this code; varchar(20)
"admin2 code",       # code for the second administrative division, a county in the US, see file admin2Codes.txt; varchar(80)
"admin3 code",       # code for third level administrative division, varchar(20)
"admin4 code",      # code for fourth level administrative division, varchar(20)
"population",        # bigint (8 byte int)
"elevation",         # in meters, integer
"dem",               # digital elevation model
"timezone",          # the iana timezone id (see file timeZone.txt) varchar(40)
"modification date") # date of last modification in yyyy-MM-dd format
​file.copy(tfile, "~/Documents/Features/GeoCategories/allCountries.zip")
head(dat.full[, c("name", "asciiname", "alternatenames")])

country.codes2 <- unique(dat.full[, "country code"])
keep.codes <- c(alpha2.code[c("United States of America (the)", "Canada",
                              "Australia", "New Zealand")],
                euro.codes)
keep.codes[!keep.codes %in% country.codes]

## TODO
## reconcile GeoNames Gazetteer data and post code data
##
