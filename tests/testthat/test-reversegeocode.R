test_that("Reverse geocoding works for entire globe",
{
    coords <- rbind(c(14.5966,120.9445),
                    c(-43.5118,172.4589))
    expected.out <- c("Philippines", "New Zealand")
    out <- ReverseGeocode(coords[,1], coords[,2], output.type = "Country")
    expect_equal(out, expected.out)

    set.seed(333)
    data(us.zip.codes, package = "flipGeoData")
    idx <- sample(nrow(us.zip.codes), 3)
    expected.out <- as.character(us.zip.codes[idx, "state"])

    out <- ReverseGeocode(us.zip.codes[idx, "latitude"], us.zip.codes[idx, "longitude"])
    expect_equal(out, expected.out)

    out <- ReverseGeocode(us.zip.codes[idx, "latitude"],
                          us.zip.codes[idx, "longitude"],
                          output.type = "Country")
    expect_equal(out, rep("United States of America", length(idx)))
})

test_that("Tolerance parameter for reverse geocoding works",
{
    coords <- c(59.6082,-15.9960) ## Faroe Islands, NE of UK
    out <- ReverseGeocode(coords[1], coords[2], tol = 100,
                          output.type = "Country")
    expect_equal(out, "Other")
    out <- ReverseGeocode(coords[1], coords[2], tol = 500,
                          output.type = "Country")
    expect_equal(out, "United Kingdom")
})
