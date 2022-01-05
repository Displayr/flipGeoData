test_that("Region detection works", {
    data(australia.post.codes, package = "flipGeoData")
    idx <- c(1599L, 12966L, 12875L, 7333L, 6618L, 14610L, 14820L, 4374L,
             15292L, 12591L, 11829L, 15356L, 9612L, 13230L, 16794L, 3145L,
             10321L, 6432L, 12844L, 6233L, 2365L, 16566L, 7264L, 3970L, 753L,
             6550L, 4441L, 12759L, 355L, 13594L, 15132L, 12546L, 5708L, 14491L,
             4498L, 2655L, 11032L, 3058L, 12089L, 7704L, 14047L, 1226L, 11646L,
             15377L, 16276L, 7081L, 15306L, 14633L, 7942L, 14135L)
    text <- australia.post.codes[idx, "suburb"]
    out <- RecodeGeography(text, output.type = "Local government area (LGA)")

    text <- c("Aylmer South",
              "North Central Island and Bute Inlet Region (Gold River)",
              "Saskatoon Northeast",
              "Etobicoke (Westmount)",
              "Centre-du-Québec-Nord (Daveluyville)",
              "Kananaskis Country (Claresholm)", "Sylvan Lake",
              "Paris", "Quebec City South",
              "South Okanagan (Summerland)", "Quebec City East",
              "Burlington South", "Châteauguay South", "Espanola",
              "Delta Central", "Lockport", "Saint-Bruno",
              "Langley Township North",
              "Scarborough (Birch Cliff / Cliffside West)",
              "Whitby Central", "Kitchener West", "Nanaimo Central",
              "Mississauga (Matheson / East Rathwood)",
              "Peterborough North", "Debec",
              "Prince Albert Central", "Petawawa")
    out <- flipGeoData:::determineGUIControlInput(text)
    expect_equal(out, c("Canada", "Place", "Postal code"))

    idx <- c(328229L, 547317L, 430800L, 62274L, 313481L, 647785L, 56543L,
             379100L, 351949L, 660222L, 330722L, 575803L, 475463L, 355318L,
             181138L, 433512L, 393703L, 473552L, 61424L, 657960L, 52082L,
             249937L, 650437L, 703110L, 617047L, 84521L, 409892L, 715458L,
             424922L, 563215L, 69760L, 547414L, 153687L, 600144L, 41853L,
             175490L, 690854L, 124972L, 568631L, 163376L, 356791L, 257322L,
             535920L, 369239L, 415194L, 15683L, 441815L, 579529L, 471518L,
             253835L, 689996L, 277130L, 450979L, 442274L, 183825L, 295216L,
             126805L, 430374L, 386925L, 120106L, 19225L, 340490L, 437490L,
             468590L, 527649L, 607916L, 106825L, 496418L, 401244L, 717863L,
             498569L, 659319L, 217638L, 214047L, 278004L, 207289L, 581058L,
             200046L, 306249L, 207230L, 376867L, 346891L, 715868L, 486621L,
             629347L, 430979L, 551687L, 73886L, 431213L, 112565L, 348854L,
             493299L, 508828L, 215899L, 344590L, 454413L, 402858L, 226473L,
             332366L, 479803L)

    data(euro.post.codes, package = "flipGeoData")
    text <- euro.post.codes[idx, "state"]
    ## Matches state first not place even though there are also matches in place
    out <- flipGeoData:::detectRegion(text, NULL, 5)
    expect_equal(out, structure("Europe", input.type = "state"))
})

test_that("Autodetect text postcodes",
{
    text <- as.character(c(3000, 2036, 6164, 3089, 3392,
                           6150, 3585, 2560, 4119, 4101))
    out <- detectRegion(text, NULL, 10)

    text <- as.character(c(11206, 10462, 11231, 11354, 11205, 10010))
    out <- determineGUIControlInput(text, 5)
    expect_equal(out, c("USA", "ZIP code", "Place"))
})

test_that("Test input type detection with min.matches works",
{
    ## three matches in place, but five in postcode --> detects postcode
    data(australia.post.codes, package = "flipGeoData")
    idx <- c(1599L, 12966L, 12875L, 7333L, 6618L, 14610L, 14820L, 4374L,
             15292L, 12591L, 11829L, 15356L, 9612L, 13230L, 16794L, 3145L,
             10321L, 6432L, 12844L, 6233L, 2365L, 16566L, 7264L, 3970L, 753L,
             6550L, 4441L, 12759L, 355L, 13594L, 15132L, 12546L, 5708L, 14491L,
             4498L, 2655L, 11032L, 3058L, 12089L, 7704L, 14047L, 1226L, 11646L,
             15377L, 16276L, 7081L, 15306L, 14633L, 7942L, 14135L)
    text <- c(australia.post.codes[idx[1:3], "post.code"],
              australia.post.codes[idx[4:8], "place"])
    out <- RecodeGeography(text, region = "Australia", output.type = "LGA")
    expected.out <- c(rep(NA_character_, 3),
                      as.character(australia.post.codes[idx[4:8], "lga"]))
    expect_equal(out, expected.out)

    ## lower min.matches, detects post.code
    out <- RecodeGeography(text, region = "Australia", output.type = "LGA",
                           min.matches = 3)
    expected.out <- c(as.character(australia.post.codes[idx[1:3], "lga"]),
                      rep(NA_character_, 5))
    expect_equal(out, expected.out)
})

test_that("Postcode to place conversion",
{
    text <- c(501, 1001)
    out <- RecodeGeography(text, region = "USA", input.type = "Postcode",
                           output.type = "State")
    expect_error(RecodeGeography(text, region = "USA", input.type = "Region",
                           output.type = "State"), "Only postcodes can be numeric.")
    data(us.zip.codes, package = "flipGeoData")
    zip <- us.zip.codes[["zip.code"]]
    idx1 <- which(zip == text[1])[1]
    idx2 <- which(zip == text[2])[1]
    out.expect <- as.character(us.zip.codes[c(idx1, idx2), "state"])
    expect_equal(out, out.expect)

    text <- c("00501", "01001")
    out <- RecodeGeography(text, region = "USA", input.type = "Postcode",
                           output.type = "Place")
    out.expect <- us.zip.codes[c(idx1, idx2), "place"]
    expect_equal(out, out.expect)
})

test_that("More stringent checking of integer postcodes",
{
    ## EU postcodes with matches in AZ and NZ
    text <- c("7000", "7071", "2443", "2485", "2491", "7011", "7012", "7013",
              "7033", "7034", "7035", "7041", "7051", "7052", "7053", "7061",
              "7062", "7063", "7064", "7072", "7081", "7082", "7083", "7091",
              "7521", "7522", "7533", "7534", "7535", "7536", "7537", "7540",
              "7542", "7543", "7544", "7551", "7552", "8291", "8292", "8293",
              "7561", "7562", "7563", "7564", "7571", "7572", "8282", "8350",
              "8380", "8382")
    out <- detectRegion(text)
    expect_equal(out, "Europe", check.attributes = FALSE)

    ## 50 NZ postcodes with 41 matches in Europe
    text <- c(9074L, 4999L, 4572L, 2155L, 9387L, 542L, 174L, 3176L, 9750L,
              4193L, 9023L, 7673L, 2697L, 981L, 5014L, 6972L, 3078L, 2473L,
              9043L, 7980L, 3025L, 9346L, 6022L, 176L, 741L, 5810L, 4884L,
              9881L, 1541L, 3243L, 7681L, 4397L, 9092L, 4815L, 2693L, 7772L,
              7348L, 2245L, 1071L, 754L, 4141L, 9048L, 4817L, 4942L, 9395L,
              9641L, 7007L, 4378L, 7198L, 281L)
    out <- RecodeGeography(text, output.type = "Region")
    data(new.zealand.post.codes, package = "flipGeoData")
    expect_true(all(out %in% levels(new.zealand.post.codes[["region"]])))
})

test_that("Approx. matching with levenshtein dist.",
{
    data(us.zip.codes, package = "flipGeoData")
    txt <- us.zip.codes[["place"]]
    idx <- c(4L, 5L, 8L, 10L)
    txt <- txt[idx]
    txt[1] <- "King Kove"
    txt[3] <- "Saint Georg Island"
    txt[4] <- "St. Paul Island"
    out <- RecodeGeography(txt, region = "USA", input.type = "Place",
                           output.type = "Zip Code",
                           max.levenshtein.dist = 0)
    expect.out <- rep(NA, length(txt))
    expect.out[2] <- as.character(us.zip.codes[idx[2], "zip.code"])
    expect_equal(out, expect.out)

    out <- RecodeGeography(txt, region = "USA", input.type = "Place",
                           output.type = "Zip Code",
                           max.levenshtein.dist = 1)
    expect.out[1:3] <- as.character(us.zip.codes[idx[1:3], "zip.code"])
    expect_equal(out, expect.out)

    out <- RecodeGeography(txt, region = "USA", input.type = "Place",
                           output.type = "Zip Code",
                           max.levenshtein.dist = 4)
    expect.out[4] <- as.character(us.zip.codes[idx[4], "zip.code"])
    expect_equal(out, expect.out)
})

test_that("Recoding works with non-title case",
{
    txt <- c("BROOKLYN", "BRONX", "BROOKLYN", "Flushing", "BROOKLYN",
             "NEW YORK", "STATEN ISLAND", "BROOKLYN", "BROOKLYN")
    out <- RecodeGeography(txt, input.type = "Place")
    txt.tcase <- flipGeoData:::convertToTitleCaseIfNecessary(txt)
    expect_equal(txt.tcase[7], "Staten Island")
    data(us.zip.codes, package = "flipGeoData")
    idx <- match(txt.tcase, us.zip.codes[["place"]])
    expect.out <- us.zip.codes[["zip.code"]][idx]
    expect_equal(out, expect.out)
})

test_that("Autodetection with non-title case",
{
    txt <- c("OTISVILLE","LAKE DELTON","CORAOPOLIS","MACON","JANESVILLE",
             "CAPE CORAL","TAMPA","GILL","WOODWARD","ORLANDO")
    expect_equal(flipGeoData:::determineGUIControlInput(txt),
                 c("USA", "Place", "ZIP code"))
})

test_that("Can find matches in neighbouring regions",
{
    txt <- c("Vancouver", "Winnipeg", "New York", "Washington", "Toronto")
    out <- RecodeGeography(txt, region = "USA", input.type = "Place",
                           output.type = "Postcode")
    data(us.zip.codes, package = "flipGeoData")
    idx <- match(txt, us.zip.codes[["place"]])
    expected.out <- as.character(us.zip.codes[idx, "zip.code"])
    expect_equal(out, expected.out)

    out <- RecodeGeography(txt, region = "USA", input.type = "Place",
                           output.type = "Postcode", check.neighboring.region = TRUE)
    txt.na <- txt[is.na(expected.out)]
    data(canada.postal.codes, package = "flipGeoData")
    out.na <- canada.postal.codes[match(txt.na, canada.postal.codes[["place"]]),
                                  "postal.code"]
    expected.out[is.na(expected.out)] <- out.na
    expect_equal(out, expected.out)

    out <- RecodeGeography(txt, region = "Canada", input.type = "Place",
                           output.type = "Postcode", check.neighboring.region = FALSE)
    idx <- match(txt, canada.postal.codes[["place"]])
    expected.out <- canada.postal.codes[idx, "postal.code"]
    expect_equal(out, expected.out)

    out <- RecodeGeography(txt, region = "Canada", input.type = "Place",
                           output.type = "Postcode", check.neighboring.region = TRUE)
    txt.na <- txt[is.na(expected.out)]
    out.na <- us.zip.codes[match(txt.na, us.zip.codes[["place"]]),
                                  "zip.code"]
    expected.out[is.na(expected.out)] <- out.na
    expect_equal(out, expected.out)

    txt <- c("Castellón", "Staffordshire", "Ourense", "Paredes", "Valongo",
             "Berkshire")
    out <- RecodeGeography(txt, region = "Europe", check.neighboring.region = TRUE)
    expect_equal(out[c(2, 5)], c("England", "England"))

    txt <- c("City Of Gosnells", "Albert-Eden", "Adelaide City Council", "Whau")
    out <- RecodeGeography(txt, region = "New Zealand", input.type = "lga",
                           output.type = "Region", check.neighboring.region = TRUE)
    expect_equal(out, c("Western Australia", "Auckland", "South Australia", "Auckland"))
})

## test_that("Recode geography finds state synonyms",
## {
##     txt <- c("MB", NA, "Alberta", "Newfoundland", "Québec")
##     expected.out <- rep(NA_character_, length(txt))
##     expected.out[3] <- "Prairies"
##     out <- RecodeGeography(txt, region = "Canada", input.type = "province",
##                            check.synonyms = FALSE)
##     expect_equal(as.character(out), expected.out)
##     expected.out <- c("Prairies", NA, "Prairies", "Atlantic", "Central")
##     out <- RecodeGeography(txt, region = "Canada", input.type = "province",
##                            check.synonyms = TRUE)
##     expect_equal(as.character(out), expected.out)

##     txt <- c("Nevada", "Nev.", "NV")
##     txt[2:3] <- flipGeoData:::findSynonymsFromList(txt[2:3], "USA")
##     expect_equal(txt, rep("Nevada", length(txt)))

##     txt <- c("Borough of Wolverhampton", "Leicester City", "Wakefield")
##     expected.out <- c("Wolverhampton")
##     out <- RecodeGeography(txt, region = "UK", input.type = "county",
##                            output.type = "district",
##                            check.synonyms = TRUE)
## })

## Test each input type and output type for each region
avail.types <- flipGeoData:::available.types
avail.types <- lapply(avail.types, function(types)
    types[!types %in% c("latitude", "longitude")])
in.idx <- c(50L, 101L, 256L, 575L, 1010L)
for (region in names(available.types))
{
    dat <- flipGeoData:::loadData(region)
    types <- avail.types[[region]]
    types <- sub("^([A-z](?:ip|ga)?)", "\\U\\1", types, perl = TRUE)
    types <- sub("\\.", " ", types, perl = TRUE)
    for (input.type in types)
    {
        cname.in <- make.names(tolower(input.type))
        otypes <- types[match(input.type, types):length(types)]
        if (grepl("^ZIP|^Post", input.type))
            otypes <- c("Place", otypes)
        for (output.type in otypes)
        {
            cname.out <- make.names(tolower(output.type))
            txt.in <- dat[in.idx, cname.in]
            match.idx <- match(txt.in, dat[[cname.in]])
            expected.out <- as.character(dat[match.idx, cname.out])
            desc <- paste0("Recode ", input.type, " to ", output.type, " for ", region)
            test_that(desc,
                      expect_equal(RecodeGeography(txt.in, region = region,
                                                   input.type = input.type, output.type = output.type),
                                   expected.out))
        }
    }
}


test_that("Can supply extra text to disambiguate places",
{
    txt <- c("Brooklyn", "Jackson", "Baltimore", "Brooklyn", "FooBar")
    txt.extra <- c("New York", "Mississippi", "Maryland", "Wisconsin", "Washington")
    out <- RecodeGeography(text = txt, text.extra = txt.extra, region = "USA",
                           input.type = "Place")
    data(us.zip.codes, package = "flipGeoData")
    tbl <- paste0(us.zip.codes[["place"]], us.zip.codes[["state"]])
    idx <- match(paste0(txt, txt.extra), tbl)
    expected.out <- us.zip.codes[idx, "zip.code"]
    expect_equal(out, as.character(expected.out))
})
