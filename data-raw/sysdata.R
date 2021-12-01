## See https://github.com/Displayr/flipStandardCharts/blob/master/R/updaterda.R
##   for how admin1.name.map is generated from http://www.naturalearthdata.com
admin1.name.map <- flipStandardCharts:::admin1.name.map

## Update spelling of an NZ region to match new.zealand.post.codes
## https://en.wikipedia.org/wiki/Whanganui#Controversy_over_Wanganui/Whanganui_spelling
nz <- admin1.name.map[["New Zealand"]]
mw.idx <- which(names(nz) %in% "Manawatu-Wanganui")
names(nz)[mw.idx] <- "Manawatu-Whanganui"
mw <- nz[[mw.idx]]
mw[nz[[mw.idx]] %in% "Manawatu Whanganui"] <- "Manawatu-Wanganui"
mw <- c(mw, "Manawatū-Whanganui", "Manawatu Wanganui")
Encoding(mw) <- "UTF-8"
nz[[mw.idx]] <- mw
admin1.name.map[["New Zealand"]] <- nz

## update name for Quebec to match canada.postal.codes data
cdn <- admin1.name.map[["Canada"]]
qb.idx <- which(startsWith(names(cdn), "Q"))
qb <- cdn[[qb.idx]]
ascii.idx <- which(qb %in% "Quebec")
qb[ascii.idx] <- names(cdn)[qb.idx]
names(cdn)[qb.idx] <- "Quebec"
cdn[[qb.idx]] <- qb
admin1.name.map[["Canada"]] <- cdn

## Update list names to match names of region arg. in RecodeGeography
map.names <- names(admin1.name.map)
map.names[map.names %in% "United Kingdom"] <- "UK"
map.names[map.names %in% "United States of America"] <- "USA"
names(admin1.name.map) <- map.names
save(admin1.name.map, file = "R/sysdata.rda", compress = TRUE)

## https://www.naturalearthdata.com/downloads/10m-cultural-vectors/10m-admin-1-states-provinces/
library(rgdal)
f <- tempfile()
download.file(file.path('http://www.naturalearthdata.com/http/',
                        'www.naturalearthdata.com/download/10m/cultural',
                        'ne_10m_admin_1_states_provinces.zip'), f)
d <- tempdir()
unzip(f, exdir = d)
d <- tempdir()
admin1.df <- readOGR(d, 'ne_10m_admin_1_states_provinces')
admin1.df <- data.frame(admin1.df)
column.class <- sapply(admin1.df, class)
column.class <- column.class[column.class == "factor"]
for (column in names(column.class))
    Encoding(levels(admin1.df[[column]])) <- "UTF-8"
keep.cols <- c("woe_name", "latitude", "longitude", "fips", "name_en",
               "gn_name", "gns_name", "region", "postal", "code_hhasc",
               "sameascity", "adm1_code", "iso_3166_2",
               "iso_a2", "adm0_sr", "name", "name_alt",
               "name_local", "type_en")
admin1 <- admin1.df[, keep.cols]

data(country.codes, package = "flipGeoData")
keep.codes <- country.codes[["country.code"]][country.codes[["post.codes.available"]]]
keep.rows <- admin1.df[["iso_a2"]] %in% keep.codes
admin1 <- admin1[keep.rows, ]

##


## USA Counties
## f <- tempfile()
## download.file(paste0("https://www.naturalearthdata.com/",
##                      "http//www.naturalearthdata.com/download/10m/cultural/",
##                      "ne_10m_admin_2_counties.zip"), f)
## unzip(f, exdir = d)
## admin2 <- readOGR(d, "ne_10m_admin_2_counties")
## admin2 <- data.frame(admin2)
## colnames(admin2) <- tolower(colnames(admin2))
## keep.rows <- admin2[["iso_a2"]] %in% keep.codes
## admin2 <- admin2[keep.rows, ]
