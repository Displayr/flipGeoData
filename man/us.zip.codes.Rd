% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/us.zip.codes.R
\docType{data}
\name{us.zip.codes}
\alias{us.zip.codes}
\title{Zip codes for the United States of America}
\format{
A data.frame with 40,970 rows and 7 columns.
}
\source{
\url{https://download.geonames.org/export/zip/}
}
\usage{
data(us.zip.codes)
}
\description{
Zip codes, approximate latitude/longitude, state, and county for 40,970 places in the United States.
}
\details{
\itemize{
\item zip.code - 3-5 digit integer zip code for \code{place}
\item place - Character; place name
\item county - Character; county or census area/2nd order administrative subdivision in the U.S.
\item state - Factor with 51 levels; state/1st order administrative subdivision in the U.S.
\item region - Factor with four levels; region of the U.S.: West, Midwest, South, or Northeast.
\item latitude - Numeric, approximate latitude for place
\item longitude - Numeric, approximate longitude for place
\item duplicate.place - Logical, indicating if \code{place} appears in more
than one state.
}
}
\keyword{datasets}
