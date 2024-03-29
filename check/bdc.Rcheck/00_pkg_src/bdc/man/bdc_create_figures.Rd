% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bdc_create_figures.R
\name{bdc_create_figures}
\alias{bdc_create_figures}
\title{Create figures reporting the results of the bdc package}
\usage{
bdc_create_figures(
  data,
  database_id = "database_id",
  workflow_step = NULL,
  save_figures = FALSE
)
}
\arguments{
\item{data}{data.frame. Containing the results of data quality tests; that
is, columns starting wit ".".}

\item{database_id}{character string. The column name with a unique record
identifier. Default = "database_id".}

\item{workflow_step}{character string. Name of the workflow step. Options
available are "prefilter", "space", and "time".}

\item{save_figures}{logical. Should the figures be saved for further
inspection? Default = FALSE.}
}
\value{
List containing figures showing the results of data quality test
implemented in one module of bdc. When save_figures = TRUE, figures are
also saved locally in a png format.
}
\description{
Creates figures (i.e., bar plots, maps, and histograms) reporting the results
of data quality tests implemented in the bdc package.
}
\details{
This function creates figures based on the results of data quality
tests implemented. A pre-defined list of test names is used for creating
figures depending on the name of the workflow step informed. Figures are
saved in "Output/Figures" if save_figures == TRUE.
}
\examples{
\dontrun{
database_id <- c("GBIF_01", "GBIF_02", "GBIF_03", "FISH_04", "FISH_05")
lat <- c(-19.93580, -13.01667, -22.34161, -6.75000, -15.15806)
lon <- c(-40.60030, -39.60000, -49.61017, -35.63330, -39.52861)
.scientificName_emptys <- c(TRUE, TRUE, TRUE, FALSE, FALSE)
.coordinates_empty <- c(TRUE, TRUE, TRUE, TRUE, TRUE)
.invalid_basis_of_records <- c(TRUE, FALSE, TRUE, FALSE, TRUE)
.summary <- c(TRUE, FALSE, TRUE, FALSE, FALSE)

x <- data.frame(
  database_id,
  lat,
  lon,
  .scientificName_emptys,
  .coordinates_empty,
  .invalid_basis_of_records,
  .summary
)

figures <- 
bdc_create_figures(
  data = x, 
  database_id = "database_id",
  workflow_step = "prefilter",
  save_figures = FALSE
)
}
}
