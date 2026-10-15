# Output contracts for every exported flag function. The contracts and the
# harness live in helper-contracts.R; each row below describes one function:
# its column-name arguments with their default names, a builder for a minimal
# valid input (compliant records first) and the column(s) it appends.

# Builds a data.frame from columns given in the order of `args`, named after
# `cols`.
cols_frame <- function(cols, args, ...) {
  stats::setNames(data.frame(..., stringsAsFactors = FALSE), cols[args])
}

contract_table <- list(
  contract_row(
    fun = "bdc_coordinates_empty",
    cols = c(lat = "decimalLatitude", lon = "decimalLongitude"),
    build = \(cols) cols_frame(
      cols, c("lat", "lon"),
      c(-15.79, NA, 10.5), c(-47.88, -40.1, NA)
    ),
    added = ".coordinates_empty",
    expected = list(.coordinates_empty = c(TRUE, FALSE, FALSE))
  ),
  contract_row(
    fun = "bdc_coordinates_outOfRange",
    cols = c(lat = "decimalLatitude", lon = "decimalLongitude"),
    build = \(cols) cols_frame(
      cols, c("lat", "lon"),
      c(-15.79, 95, 10.5), c(-47.88, -40.1, 190)
    ),
    added = ".coordinates_outOfRange",
    expected = list(.coordinates_outOfRange = c(TRUE, FALSE, FALSE))
  ),
  contract_row(
    fun = "bdc_coordinates_precision",
    cols = c(lat = "decimalLatitude", lon = "decimalLongitude"),
    build = \(cols) cols_frame(
      cols, c("lat", "lon"),
      c(-15.794, -15.7, -15), c(-47.882, -47.882, -47)
    ),
    added = ".rou",
    expected = list(.rou = c(TRUE, FALSE, FALSE))
  ),
  contract_row(
    fun = "bdc_basisOfRecords_notStandard",
    cols = c(basisOfRecord = "basisOfRecord"),
    build = \(cols) cols_frame(
      cols, "basisOfRecord",
      c("PreservedSpecimen", "HUMAN_OBSERVATION", "FOSSIL_SPECIMEN")
    ),
    added = ".basisOfRecords_notStandard",
    expected = list(.basisOfRecords_notStandard = c(TRUE, TRUE, FALSE))
  ),
  contract_row(
    fun = "bdc_scientificName_empty",
    cols = c(sci_names = "scientificName"),
    build = \(cols) cols_frame(
      cols, "sci_names",
      c("Puma concolor", NA, "  ")
    ),
    added = ".scientificName_empty",
    expected = list(.scientificName_empty = c(TRUE, FALSE, FALSE))
  ),
  contract_row(
    fun = "bdc_eventDate_empty",
    cols = c(eventDate = "eventDate"),
    build = \(cols) cols_frame(
      cols, "eventDate",
      c("2001-05-20", NA, "-")
    ),
    added = ".eventDate_empty",
    expected = list(.eventDate_empty = c(TRUE, FALSE, FALSE))
  ),
  contract_row(
    fun = "bdc_year_from_eventDate",
    cols = c(eventDate = "eventDate"),
    build = \(cols) cols_frame(
      cols, "eventDate",
      c("2001-05-20", "20/05/1999", "no date")
    ),
    added = "year",
    flags = character(),
    expected = list(year = c(2001, 1999, NA))
  ),
  contract_row(
    fun = "bdc_year_outOfRange",
    cols = c(eventDate = "eventDate"),
    build = \(cols) cols_frame(
      cols, "eventDate",
      c("2001-05-20", "1550", "2999")
    ),
    added = ".year_outOfRange",
    expected = list(.year_outOfRange = c(TRUE, FALSE, FALSE))
  ),
  contract_row(
    fun = "bdc_coordinates_from_locality",
    cols = c(
      lat = "decimalLatitude", lon = "decimalLongitude", locality = "locality"
    ),
    # Returns, by design, only the records lacking valid coordinates that
    # have a locality: rows 2 and 4.
    build = \(cols) cols_frame(
      cols, c("lat", "lon", "locality"),
      c(-15.79, NA, NA, 95),
      c(-47.88, NA, NA, -40.1),
      c("Brasilia", "Parque Nacional Iguazu", "", "Serra do Cipo")
    ),
    keep = c(2L, 4L),
    added = c(".coordinates_empty", ".coordinates_outOfRange"),
    expected = list(
      .coordinates_empty = c(FALSE, TRUE),
      .coordinates_outOfRange = c(TRUE, FALSE)
    ),
    gaps = list(custom_names = "ignores a custom `locality` column (#326)")
  ),
  contract_row(
    fun = "bdc_country_standardized",
    cols = c(country = "country"),
    build = \(cols) cols_frame(
      cols, "country",
      c("Brazil", "brasil", "Atlantis")
    ),
    added = c("country_suggested", "countryCode"),
    flags = character(),
    expected = list(
      country_suggested = c("Brazil", "Brazil", NA),
      countryCode = c("BR", "BR", NA)
    ),
    requires = \() skip_if_not_installed("rnaturalearthdata")
  ),

  # Spatial checks: enabled by #305, which verifies them against the contracts
  # now that their column-name (#293) and session-state (#292) fixes landed.
  contract_row(
    fun = "bdc_coordinates_country_inconsistent",
    cols = c(
      country = "country_suggested",
      lat = "decimalLatitude", lon = "decimalLongitude"
    ),
    # Brasilia, then Buenos Aires claimed as Brazil.
    build = \(cols) cols_frame(
      cols, c("country", "lat", "lon"),
      c("Brazil", "Brazil"), c(-15.79, -34.60), c(-47.88, -58.38)
    ),
    args = list(country_name = "Brazil"),
    added = ".coordinates_country_inconsistent",
    expected = list(.coordinates_country_inconsistent = c(TRUE, FALSE)),
    requires = skip_if_no_natural_earth,
    skip = "spatial row, enabled by #305 (fixes #292, #293)"
  ),
  contract_row(
    fun = "bdc_country_from_coordinates",
    cols = c(
      lat = "decimalLatitude", lon = "decimalLongitude", country = "country"
    ),
    # A missing country is filled from the coordinates; a known one is kept.
    build = \(cols) cols_frame(
      cols, c("lat", "lon", "country"),
      c(-15.79, -34.60), c(-47.88, -58.38), c(NA, "Argentina")
    ),
    added = character(),
    modifies = "country",
    expected = list(country = c("Brazil", "Argentina")),
    requires = skip_if_no_natural_earth,
    skip = "spatial row, enabled by #305 (fixes #292, #293)"
  ),
  contract_row(
    fun = "bdc_coordinates_transposed",
    cols = c(
      id = "database_id", sci_names = "scientificName",
      lat = "decimalLatitude", lon = "decimalLongitude",
      country = "country", countryCode = "countryCode"
    ),
    # Only the first record has coordinates in the right order; the others
    # are transposed and get corrected in place.
    build = \(cols) cols_frame(
      cols, c("id", "sci_names", "lat", "lon", "country", "countryCode"),
      paste0("rec_", 1:4),
      c(
        "Scinax ruber", "Rhinella major",
        "Siparuna guianensis", "Psychotria vellosiana"
      ),
      c(-14.43333, 63.43333, -41.90000, -46.69778),
      c(-67.91667, -17.90000, -13.25000, -13.82444),
      c("Bolivia", "Bolivia", "Brazil", "Brazil"),
      c("BO", "BO", "BR", "BR")
    ),
    added = "coordinates_transposed",
    modifies = c("lat", "lon"),
    expected = list(coordinates_transposed = c(TRUE, FALSE, FALSE, FALSE)),
    args = list(save_outputs = FALSE),
    requires = skip_if_no_natural_earth,
    skip = "spatial row, enabled by #305 (fixes #292, #293)"
  )
)

for (row in contract_table) expect_output_contracts(row)
