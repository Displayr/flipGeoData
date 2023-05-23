## https://www.naturalearthdata.com/downloads/10m-cultural-vectors/10m-admin-1-states-provinces/
library(rgdal)
f <- tempfile()
download.file(file.path('https://www.naturalearthdata.com/http/',
                        'www.naturalearthdata.com/download/10m/cultural',
                        'ne_10m_admin_1_states_provinces.zip'), f)
d <- tempdir()
unzip(f, exdir = d)
d <- tempdir()
admin1.coordinates <- readOGR(d, 'ne_10m_admin_1_states_provinces', stringsAsFactors = TRUE)
admin1.df <- data.frame(admin1.coordinates)
column.class <- sapply(admin1.df, class)
column.class <- column.class[column.class == "factor"]
for (column in names(column.class))
    Encoding(levels(admin1.coordinates[[column]])) <- "UTF-8"
keep.cols <- c("woe_name", "latitude", "longitude", "fips", "name_en",
               "gn_name", "gns_name", "region", "postal", "code_hasc",
               "sameascity", "adm1_code", "iso_3166_2",
               "iso_a2", "adm0_sr", "name", "name_alt",
               "name_local", "type_en")
admin1 <- admin1.df[, keep.cols]

data(country.codes, package = "flipGeoData")
keep.codes <- country.codes[["country.code"]][country.codes[["post.codes.available"]]]
keep.rows <- admin1.df[["iso_a2"]] %in% keep.codes
admin1 <- admin1[keep.rows, ]

createSynonymsTable <- function(region)
{
    dat <- flipGeoData:::loadData(region)
    ccode <- flipGeoData:::regionToCountryCode(region)
    idx <- admin1[["iso_a2"]] %in% ccode
    admin1.synonyms <- admin1[idx, ]
    admin1.synonyms[["iso3"]] <- if (region %in% c("New Zealand", "Australia"))
                                     sub("^[A-Z]{2}-(?!X[0-9])", "",
                                         admin1.synonyms[["iso_3166_2"]], perl = TRUE)
                                  else NA
    synonyms <- t(apply(admin1.synonyms[, c("name_alt", "name_en", "name", "postal",
                                            "iso_3166_2", "iso3", "gn_name", "gns_name")], 1,
                   function(name.vec){
                       alts <- if (!is.na(name.vec[1]))unlist(strsplit(name.vec[1], "|", fixed = TRUE))
                       name <- name.vec[2]

                       name.vec <- unique(c(alts, name.vec[-1]))
                       name.vec <- name.vec[!is.na(name.vec)]
                       name.vec <- name.vec[!name.vec == name]
                       return(c(name, paste(name.vec, collapse = "|")))
                   }))
    colnames(synonyms) <- c("name", "synonyms")
    synonyms <- as.data.frame(synonyms)
    synonyms[["type"]] <- switch(region, USA = "state", Canada = "province",
                                 Australia = "state", "New Zealand" = "region")
    if (region %in% c("Canada", "USA"))
    {
        cname <- ifelse(region == "Canada", "province", "state")
        idx <- vapply(synonyms[["name"]], match, 1L,
                      table = dat[[cname]])
        synonyms[["merged"]] <- as.character(dat[idx, "region"])
    }else
        synonyms[["merged"]] <- NA

    synonyms[["latitude"]] <- admin1.synonyms[["latitude"]]
    synonyms[["longitude"]] <- admin1.synonyms[["longitude"]]
    synonyms[["region"]] <- region
    return(synonyms)
}



## For Europe and UK we additionally check that the synonym occurs in the data

library(fastmatch)
findAndCreateSynonymsTable <- function(region)
{
    dat <- flipGeoData:::loadData(region)
    ccode <- flipGeoData:::regionToCountryCode(region)
    if (region == "Europe")
        ccode <- ccode[!ccode %in% "GB"]
    idx <- admin1[["iso_a2"]] %in% ccode
    admin1.synonyms <- admin1[idx, ]
    synonyms <- t(apply(admin1.synonyms[, c("name_alt", "name", "name_en",
                                            "woe_name", "gn_name", "gns_name")], 1,
                        function(name.vec){
                       alts <- if (!is.na(name.vec[1]))unlist(strsplit(name.vec[1], "|", fixed = TRUE))
                       name.vec <- unique(c(alts, name.vec[-1]))
                       name.vec <- name.vec[!is.na(name.vec)]
                       cidx <- grep("province|district", colnames(dat), value = TRUE)
                       matches <- fmatch(name.vec, dat[[cidx]])
                       synonyms <- paste(name.vec, collapse = "|")
                       if (all(is.na(matches)))
                       {
                           cidx <- grep("county|state", colnames(dat), value = TRUE)
                           matches <- fmatch(name.vec, dat[[cidx]])
                           if (all(is.na(matches))){
                               matches <- fmatch(name.vec, dat[["place"]])
                               if (all(is.na(matches))){
                                   return(c(NA_character_, synonyms, NA_character_))
                               }else{
                                   idx <- matches[!is.na(matches)][1]
                                   return(c(dat[idx, "place"],
                                            synonyms, "place"))

                               }

                           }else{
                               idx <- matches[!is.na(matches)][1]
                               return(c(as.character(dat[idx, cidx]),
                                        synonyms, cidx))
                           }

                       }else{
                           idx <- matches[!is.na(matches)][1]
                           return(c(as.character(dat[idx, cidx]),
                                    synonyms, cidx))

                       }
                   }))
    synonyms[, 2] <- paste(synonyms[, 2], admin1.synonyms[["iso_3166_2"]],
                           admin1.synonyms[["code_hasc"]], sep = "|")
    colnames(synonyms) <- c("name", "synonyms", "type")
    synonyms <- as.data.frame(synonyms)
    synonyms[["merged"]] <- admin1.synonyms[, "iso_a2"]
    synonyms[["latitude"]] <- admin1.synonyms[, "latitude"]
    synonyms[["longitude"]] <- admin1.synonyms[, "longitude"]
    non.na.idx <- !is.na(synonyms[, 1])
    print(sprintf("Region %s: %i out of %i synonyms found.", region, sum(non.na.idx),
            length(non.na.idx)))
    synonyms <- synonyms[non.na.idx, ]
    synonyms[["region"]] <- region
    return(synonyms)
}

regions <- c("USA", "Canada", "Australia", "New Zealand")
region.dfs <- lapply(regions, createSynonymsTable)
names(region.dfs) <- regions
## Fix D.C. to match us.zip.codes data
region <- "USA"
idx <- grep("District of Columbia", region.dfs[[region]]$synonyms)
region.dfs[[region]]$name[idx] <- "District of Columbia"
region.dfs[[region]]$synonyms[idx] <- sub("District of Columbia", "Washington DC|Washington D.C.",
                                region.dfs[[region]]$synonyms[idx])
## Fix Manawatu-Whanganui to match new.zealand.post.codes data
region <- "New Zealand"
idx <- grep("Manawatu", region.dfs[[region]]$synonyms)
region.dfs[[region]]$synonyms[idx] <- paste0(region.dfs[[region]]$name[idx], "|",
                                             region.dfs[[region]]$synonyms[idx])
region.dfs[[region]]$name[idx] <- "Manawatu-Whanganui"

admin1.synonyms <- do.call(rbind, region.dfs)
admin1.synonyms <- rbind(admin1.synonyms, findAndCreateSynonymsTable("Europe"))
admin1.synonyms <- rbind(admin1.synonyms, findAndCreateSynonymsTable("UK"))
admin1.synonyms <- do.call(rbind, c(region.dfs, findAndCreateSynonymsTable("Europe"),
                             findAndCreateSynonymsTable("UK")))
rownames(admin1.synonyms) <- NULL
admin1.synonyms[["type"]] <- as.factor(admin1.synonyms[["type"]])
admin1.synonyms[["merged"]] <- as.factor(admin1.synonyms[["merged"]])
admin1.synonyms[["region"]] <- as.factor(admin1.synonyms[["region"]])

save(admin1.synonyms, admin1.coordinates, file = "R/sysdata.rda", compress = TRUE)
