test_that("Region detection works", {
    data(australia.post.codes, package = "flipGeoData")
    idx <- c(1599L, 12966L, 12875L, 7333L, 6618L, 14610L, 14820L, 4374L,
             15292L, 12591L, 11829L, 15356L, 9612L, 13230L, 16794L, 3145L,
             10321L, 6432L, 12844L, 6233L, 2365L, 16566L, 7264L, 3970L, 753L,
             6550L, 4441L, 12759L, 355L, 13594L, 15132L, 12546L, 5708L, 14491L,
             4498L, 2655L, 11032L, 3058L, 12089L, 7704L, 14047L, 1226L, 11646L,
             15377L, 16276L, 7081L, 15306L, 14633L, 7942L, 14135L)
    text <- australia.post.codes[idx, "suburb"]
    out <- RecodeGeography(text, output.type = "LGA")

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

test_that("Test input type detection works",
{
    MIN.MATCHES  <- 5  # set in detectRegion()
    ## three matches in state, but five in place --> detects place
    data(australia.post.codes, package = "flipGeoData")
    idx <- c(1599L, 12966L, 12875L, 7333L, 6618L, 14610L, 14820L, 4374L,
             15292L, 12591L, 11829L, 15356L, 9612L, 13230L, 16794L, 3145L,
             10321L, 6432L, 12844L, 6233L, 2365L, 16566L, 7264L, 3970L, 753L,
             6550L, 4441L, 12759L, 355L, 13594L, 15132L, 12546L, 5708L, 14491L,
             4498L, 2655L, 11032L, 3058L, 12089L, 7704L, 14047L, 1226L, 11646L,
             15377L, 16276L, 7081L, 15306L, 14633L, 7942L, 14135L)
    text <- c(australia.post.codes[idx[1:3], "place"],
              australia.post.codes[idx[4:8], "post.code"])
    out <- RecodeGeography(text, region = "Australia", output.type = "LGA")
    expected.out <- c(rep(NA_character_, 3),
                      as.character(australia.post.codes[idx[4:8], "lga"]))
    expect_equal(out, expected.out)

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
    expect.out[2] <- us.zip.codes[idx[2], "zip.code"]
    expect_equal(out, expect.out)

    out <- RecodeGeography(txt, region = "USA", input.type = "Place",
                           output.type = "Zip Code",
                           max.levenshtein.dist = 1)
    expect.out[1:3] <- us.zip.codes[idx[1:3], "zip.code"]
    expect_equal(out, expect.out)

    out <- RecodeGeography(txt, region = "USA", input.type = "Place",
                           output.type = "Zip Code",
                           max.levenshtein.dist = 4)
    expect.out[4] <- us.zip.codes[idx[4], "zip.code"]
    expect_equal(out, expect.out)
})

test_that("Recoding works with non-title case",
{
    txt <- c("BROOKLYN", "BRONX", "BROOKLYN", "Flushing", "BROOKLYN",
             "NEW YORK", "STATEN ISLAND", "BROOKLYN", "BROOKLYN")
    ## out <- RecodeGeography(txt, input.type = "Place")
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
