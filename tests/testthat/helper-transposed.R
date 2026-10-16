# Four records for bdc_coordinates_transposed(): the first one has transposed
# coordinates. Shared by test-bdc_create_report.R and
# test-bdc_create_figures.R, which read the check file it writes.

transposed_input <- data.frame(
  database_id = c("gbif_1", "gbif_2", "gbif_3", "gbif_4"),
  scientificName = c(
    "Rhinella major", "Scinax ruber",
    "Siparuna guianensis", "Psychotria vellosiana"
  ),
  decimalLatitude = c(63.43333, -14.43333, -41.90000, -46.69778),
  decimalLongitude = c(-17.90000, -67.91667, -13.25000, -13.82444),
  country_suggested = c("Bolivia", "Bolivia", "Brazil", "Brazil"),
  countryCode = c("BO", "BO", "BR", "BR"),
  .scientificName_empty = c(TRUE, FALSE, TRUE, TRUE)
)

flag_transposed <- function(data) {
  suppressMessages(
    bdc_coordinates_transposed(
      data = data,
      id = "database_id",
      sci_names = "scientificName",
      lat = "decimalLatitude",
      lon = "decimalLongitude",
      country = "country_suggested",
      countryCode = "countryCode",
      border_buffer = 0.2,
      save_outputs = TRUE
    )
  )
}
