#' Reverse geocode latitude and longitude coordinates
#'
#' Converts input vectors of latitude and longitude coordinates to
#' their corresponding country or administrative region level 1
#' (state, province, etc.) within the country.
#' @param latitude Numeric vector of latitude coordinates (should be
#'     between -90 and 90).
#' @param longitude Numeric vector of longitude coordinates (should be
#'     between -180 and 180).
#' @param output.type String; either \code{"Country"} to return
#'     country names or \code{"Admin1"} (the default) to return
#'     state/province names.
#' @param tol Number giving the maximum distance in kilometers a point
#'     may be from a region/polygon and still be declared a
#'     match. Defaults to 100km, set to 0 to require all matches to be
#'     inside the nearest polygon.
#' @return A character vector containing the country or adminstrative
#'     region for each latitude-longitude pair. \code{"Other"} is
#'     returned for points outside where no matching country or region
#'     is found.
#' @importFrom sp SpatialPoints over CRS proj4string
#' @importFrom sf st_intersects st_as_sf st_length st_nearest_points
#'     st_geometry st_join sf_use_s2
#' @importFrom units set_units
#' @examples
#' ReverseGeocode(c(43.649, 37.768), c(-72.319, -75.666))
#' ReverseGeocode(c(47.906, 59.932), c(15.239, 30.197), output.type = "Country")
#' ## Lighthouse just off the BC coast
#' ReverseGeocode(48.3917338,-123.3702094)
#' ReverseGeocode(48.3917338,-123.3702094, tol = 0)
#' @export
ReverseGeocode <- function(latitude, longitude, output.type = c("Admin1", "Country"), tol = set_units(100, "km"))
{
    tol <- set_units(tol, "km")
    output.type <- match.arg(output.type)

    sp.args <- CRS(suppressWarnings(proj4string(admin1.coordinates)))
    coords.sp <- SpatialPoints(cbind(longitude, latitude), proj4string = sp.args)

    ## avoid error about invalid spherical geometry for one state in Brazil
    sf_use_s2(FALSE)
    coords.sf  <- suppressMessages(st_as_sf(coords.sp))
    admin1.sf <- suppressMessages(st_as_sf(admin1.coordinates))

    ## matches <- over(coords.sp, admin1.coordinates)
    matches.sf <- st_join(coords.sf, admin1.sf, join = sfNearestWithTolerance, tol = tol)

    cname <- ifelse(output.type == "Country", "admin", "name")
    out <- as.character(matches.sf[[cname]])
    out[is.na(out)] <- "Other"
    return(out)
}

#' Extention of sf_nearest_points with a tolerance parameter
#'
#' Computes nearest polygon for a set of latitude-longitude
#' coordinates, returning NA for points where the nearest polygon is
#' further away than the specified tolerance.
#' @param pts An sf data.frame of points to search for
#' @param polys An sf data.frame of polygons
#' @param tol maximum allowed distance in km (from a call to
#'     set_units) for a match to be declared
#' @return Integer vector with length equal to number of rows in
#'     \code{pts} giving the row index in \code{polys} of the polygon
#'     containing or nearest to the entry in \code{pts}
#' @references \url{https://stackoverflow.com/a/51713191/3008338}
#' @noRd
sfNearestWithTolerance <- function(pts, polys, tol)
{
    matches <- as.integer(suppressMessages(st_intersects(pts, polys)))
    if (anyNA(matches))
    {
        na.idx <- which(is.na(as.integer(matches)))
        nearest.len <- integer(length(na.idx))
        pts_geo <- st_geometry(pts)
        for (i in na.idx)
        {
            nearest.idx <- suppressMessages(st_nearest_points(pts_geo[i], polys))
            nearest.len <- st_length(nearest.idx)
            min.idx <- which.min(nearest.len)
            if (nearest.len[min.idx] <= tol)
                matches[i] <- min.idx
        }
    }
    return(matches)
}
