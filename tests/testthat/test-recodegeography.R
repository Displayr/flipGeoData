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
    out.expect <- us.zip.codes[c(idx1, idx2), "state"]
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
