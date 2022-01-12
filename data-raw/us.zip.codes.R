fname <- "US.zip"
dfile <- tempfile()
if (!file.exists(fname))
{
    url <- paste0("https://download.geonames.org/export/zip/", fname)
    download.file(url, dfile)
}
tdir <- tempdir()
unzip(dfile, exdir = tdir)

fname <- sub("zip$", "txt", fname)
dat.full <- read.delim(file.path(tdir, fname),
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

## Add region info using sysdata from flipStandardCharts
us.regions <- structure(list(
    RegionNumber = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L,
                               2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L,
                               3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
                               3L, 3L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L,
                               4L, 4L, 4L), .Label = c("1", "2", "3", "4"),
                             class = "factor"),
    DivisionNumber = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L,
                                 3L, 3L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 5L, 5L, 5L,
                                 5L, 5L, 5L, 5L, 5L, 5L, 6L, 6L, 6L, 6L, 7L, 7L,
                                 7L, 7L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 9L, 9L,
                                 9L, 9L, 9L), .Label = c("1", "2", "3", "4", "5",
                                                         "6", "7", "8", "9"),
                               class = "factor"),
    Region = structure(c(2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L,
                         1L, 1L, 1L, 1L, 1L, 1L, 1L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
                         3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 4L,
                         4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L),
                       .Label = c("Midwest", "Northeast", "South", "West"),
                       class = "factor"),
    Division = structure(c(5L, 5L, 5L, 5L, 5L, 5L, 3L, 3L, 3L, 1L, 1L, 1L, 1L, 1L,
                           8L, 8L, 8L, 8L, 8L, 8L, 8L, 7L, 7L, 7L, 7L, 7L, 7L, 7L,
                           7L, 7L, 2L, 2L, 2L, 2L, 9L, 9L, 9L, 9L, 4L, 4L, 4L, 4L,
                           4L, 4L, 4L, 4L, 6L, 6L, 6L, 6L, 6L),
                         .Label = c("East North Central", "East South Central",
                                    "Mid-Atlantic", "Mountain", "New England",
                                    "Pacific", "South Atlantic", "West North Central",
                                    "West South Central"), class = "factor"),
    State = structure(c(7L, 20L, 22L, 30L, 40L, 46L, 31L, 33L, 39L, 14L, 15L, 23L,
                        36L, 50L, 16L, 17L, 24L, 26L, 28L, 35L, 42L, 8L, 10L, 11L,
                        21L, 34L, 41L, 47L, 9L, 49L, 1L, 18L, 25L, 43L, 4L, 19L,
                        37L, 44L, 3L, 6L, 13L, 27L, 29L, 32L, 45L, 51L, 2L, 5L,
                        12L, 38L, 48L),
                      .Label = c("Alabama", "Alaska", "Arizona", "Arkansas",
                                 "California", "Colorado", "Connecticut", "Delaware",
                                 "District of Columbia", "Florida", "Georgia",
                                 "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa",
                                 "Kansas", "Kentucky", "Louisiana", "Maine",
                                 "Maryland", "Massachusetts", "Michigan", "Minnesota",
                                 "Mississippi", "Missouri", "Montana", "Nebraska",
                                 "Nevada", "New Hampshire", "New Jersey", "New Mexico",
                                 "New York", "North Carolina", "North Dakota",
                                 "Ohio", "Oklahoma", "Oregon", "Pennsylvania",
                                 "Rhode Island", "South Carolina", "South Dakota",
                                 "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
                                 "Washington", "West Virginia", "Wisconsin", "Wyoming"),
                      class = "factor"),
    Code = structure(c(7L, 21L, 19L, 30L, 39L, 46L, 31L, 34L, 38L, 21L, 15L, 22L,
                       35L, 48L, 13L, 16L, 23L, 24L, 29L, 28L, 41L, 9L, 10L, 11L,
                       20L, 27L, 40L, 45L, 8L, 49L, 2L, 17L, 25L, 42L, 3L, 18L,
                       36L, 43L, 4L, 6L, 14L, 26L, 33L, 32L, 44L, 50L, 1L, 5L,
                       12L, 37L, 47L),
                     .Label = c("AK","AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE",
                                "FL", "GA", "HI", "IA", "ID", "IN", "KS", "KY",
                                "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT",
                                "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY",
                                "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN",
                                "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY"),
                     class = "factor")),
    .Names = c("RegionNumber", "DivisionNumber", "Region", "Division", "State", "Code"),
    row.names = c(NA, 51L), class = "data.frame")
region <- as.character(us.regions[["Region"]])
names(region) <- as.character(us.regions[["State"]])
us.zip.codes$region <- dplyr::recode(us.zip.codes[["state"]], !!!region)

us.zip.codes <- us.zip.codes[, c("place", "zip.code", "region", "state",
                                 "county", "latitude", "longitude")]

## add column to indicate if should disambiguate place name by appending state
## when converting postcodes to place names
## A place may have multiple entries in the data.frame if
## it has multiple ZIP codes, but it shouldn't be considered
## a duplicate if it only occurs in one state.
place <- us.zip.codes[["place"]]
state <- us.zip.codes[["state"]]
## dup.p <- duplicated(place) |
##     duplicated(place, fromLast = TRUE)
## p.and.s <- paste(place, us.zip.codes[["state"]], collapse = ", ")
## potential.dup <- dup.p & !(duplicated(p.and.s) | duplicated(p.and.s, fromLast = TRUE))
potential.dup <- logical(length(place))
for (p in unique(place))
{
    p.idx <- which(place == p)
    if (length(p.idx) == 1L || length(unique(state[p.idx])) == 1L)
        next
    potential.dup[p.idx] <- TRUE
}
us.zip.codes$duplicate.place <- potential.dup

save(us.zip.codes, file = "data/us.zip.codes.rda", compress = TRUE)

## Local Variables:
## ess-r-package--project-cache: (flipGeoData . "~/flip/flipGeoData/")
## End:
