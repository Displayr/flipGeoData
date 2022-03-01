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

test_that("Reverse geocoding handles missing data and invalid coordinates",
{
    coords <- cbind(c(14.5966, NA, -43.5118, -43.5118, 83.6, NA, 105),
                    c(120.9445, 172.4589, 172.4589, NA, -200, NA, 102))
    expected.out <- c("Philippines", "Other", "New Zealand", rep("Other", 4))
    expect_warning(out <- ReverseGeocode(coords[,1], coords[,2],
                                         output.type = "Country"), NA)
    expect_equal(out, expected.out)
})
