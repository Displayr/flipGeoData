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
