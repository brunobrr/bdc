% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bdc_coordinates_transposed.R
\name{bdc_coordinates_transposed}
\alias{bdc_coordinates_transposed}
\title{Identify transposed geographic coordinates}
\usage{
bdc_coordinates_transposed(
  data,
  id = "database_id",
  sci_names = "scientificName",
  lat = "decimalLatitude",
  lon = "decimalLongitude",
  country = "country",
  countryCode = "countryCode",
  border_buffer = 0.2,
  save_outputs = FALSE
)
}
\arguments{
\item{data}{data.frame. Containing a unique identifier for each record,
geographical coordinates, and country names. Coordinates must be expressed
in decimal degrees and WGS84.}

\item{id}{character string. The column name with a unique record identifier.
Default = "database_id".}

\item{sci_names}{character string. The column name with species scientific
name. Default = "scientificName".}

\item{lat}{character string. The column name with latitude. Coordinates must
be expressed in decimal degrees and WGS84. Default = "decimalLatitude".}

\item{lon}{character string. The column with longitude. Coordinates must be
expressed in decimal degrees and WGS84. Default = "decimalLongitude".}

\item{country}{character string. The column name with the country
assignment of each record. Default = "country".}

\item{countryCode}{character string. The column name with an ISO-2 country
code.}

\item{border_buffer}{numeric >= 0. A distance in decimal degrees used to
created a buffer around the country. Records within a given country and at
a specified distance from the border will be not be corrected.
Default = 0.2 (~22 km at the equator).}

\item{save_outputs}{logical. Should a table containing transposed coordinates
saved for further inspection? Default = FALSE.}
}
\value{
A data.frame containing the column "coordinates_transposed"
indicating if verbatim coordinates were not transposed (TRUE). Otherwise
records are flagged as (FALSE) and, in this case, verbatim coordinates are
replaced by corrected coordinates.
}
\description{
This function flags and corrects records when latitude and longitude appear
to be transposed.
}
\details{
This test identifies transposed coordinates resulted from mismatches
between the country informed for a record and coordinates. Transposed
coordinates often fall outside of the indicated country (i.e., in other
countries or in the sea). Different coordinate transformations are
performed to correct country/coordinates mismatches. Importantly, verbatim
coordinates are replaced by the corrected ones in the returned database. A
database containing verbatim and corrected coordinates is created in
"Output/Check/01_coordinates_transposed.csv" if save_outputs == TRUE. The
columns "country" and "countryCode" can be retrieved by using the function
\code{\link{bdc_country_standardized}}.
}
\examples{
\dontrun{
id <- c(1, 2, 3, 4)
scientificName <- c(
  "Rhinella major", "Scinax ruber",
  "Siparuna guianensis", "Psychotria vellosiana"
)
decimalLatitude <- c(63.43333, -14.43333, -41.90000, -46.69778)
decimalLongitude <- c(-17.90000, -67.91667, -13.25000, -13.82444)
country <- c("BOLIVIA", "bolivia", "Brasil", "Brazil")

x <- data.frame(
  id, scientificName, decimalLatitude,
  decimalLongitude, country
)

# Get country code
x <- bdc_country_standardized(data = x, country = "country")

bdc_coordinates_transposed(
  data = x,
  id = "id",
  sci_names = "scientificName",
  lat = "decimalLatitude",
  lon = "decimalLongitude",
  country = "country_suggested",
  countryCode = "countryCode",
  border_buffer = 0.2,
  save_outputs = FALSE 
) 
}

}
\seealso{
Other prefilter: 
\code{\link{bdc_basisOfRecords_notStandard}()},
\code{\link{bdc_coordinates_country_inconsistent}()},
\code{\link{bdc_coordinates_empty}()},
\code{\link{bdc_coordinates_from_locality}()},
\code{\link{bdc_coordinates_outOfRange}()},
\code{\link{bdc_country_standardized}()},
\code{\link{bdc_scientificName_empty}()}
}
\concept{prefilter}
