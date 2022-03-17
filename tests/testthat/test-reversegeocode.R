test_that("Reverse geocoding works for entire globe",
{
    coords <- rbind(c(14.5966,120.9445),
                    c(-43.5118,172.4589))
    expected.out <- c("Philippines", "New Zealand")
    out <- ReverseGeocode(coords[,1], coords[,2], output.type = "Country")
    expect_equal(out, expected.out)
    coords <- as.character(coords)
    dim(coords) <- c(2, 2)
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

test_that("Fallback to sp::over if st_join with tolerance fails",
{
    df <- structure(list(latitude = structure(c(52.5011, 52.4385, 52.6587,
52.1686, 51.1375, 51.4065, 55.917, 52.2747, 55.6298, 55.7757,
53.3815, 52.2724, 52.6609, 51.4269, 50.981, 52.5994, 51.5948,
51.2643, 54.5521, 52.255, 45.2667, 38.7968, 37.1299, 38.2795,
41.5329, 42.7168, 61.5439, 52.1333, 37.695, 41.4865, 46.4832,
37.9655, 38.7833, 41.3868, 50.2833, 41.2518, 60.3984, 37.75,
40.0932, 44.3819), questiontype = "Number", dataset = "uk_and_eu", codeframe = list(
    latitude = 0L), name = "latitude", label = "latitude", question = "latitude"),
    longitude = structure(c(-1.4489, -1.5279, -2.0143, 0.1018,
    0.0083, -0.0898, -4.2159, -1.5359, -4.6167, -4.0362, -1.3995,
    -0.9174, -2.4789, -0.4271, -1.3912, -2.1355, -0.0251, 0.5621,
    -1.1914, -0.9036, 27.9833, -9.471, -8.1626, 35.6372, -6.7339,
    -6.5798, 5.1796, 14.3333, 41.7925, -8.3174, 30.2168, -8.0866,
    -9.35, -8.3394, 16.05, 15.1691, 5.3257, -25.2833, -8.3326,
    26.1227), questiontype = "Number", dataset = "uk_and_eu", codeframe = list(
        longitude = 0L), name = "longitude", label = "longitude", question = "longitude")), class = "data.frame", row.names = c(NA,
                                                                                                                                -40L))
    expect_warning(out <- ReverseGeocode(df$latitude, df$longitude), "Setting tolerance to 0km")
    expect_equal(out[1], "Warkwickshire")
    expect_length(out, nrow(df))
})
