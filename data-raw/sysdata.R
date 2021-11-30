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
