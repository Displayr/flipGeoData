% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/uk.post.codes.R
\docType{data}
\name{uk.post.codes}
\alias{uk.post.codes}
\title{Postcodes for the United Kingdom}
\format{
A data.frame with 1,791,546 rows and 7 columns.
}
\source{
\url{https://download.geonames.org/export/zip/}
}
\usage{
data(uk.post.codes)
}
\description{
Postcode unit, approximate latitude/longitude, county, and district/community info for countries in the United Kingdom.
}
\details{
\itemize{
\item post.code - string with 6-8 characters providing the full postcode unit
(incode and outcode) for \code{place}
\item place - character string place name
\item district - Character; community/district/3rd order administrative subdivision
\item county - Factor with 92 levels; county/2nd order administrative subdivision
\item region - Factor with 14 levels; one of the nine regions of England or identical
to \code{country} if \code{place} is outside England.
\item country - Factor with six levels; country/1st order administrative subdivision
\item latitude - Numeric, approximate latitude for region/place
\item longitude - Numeric, approximate longitude for region/place
\item duplicate.place - Logical, indicating if \code{place} appears in more
than one region
}
}
\keyword{datasets}
