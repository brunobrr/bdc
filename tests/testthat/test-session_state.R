skip_on_cran()

# bdc functions must leave the user's session as they found it: no packages
# attached to the search path and the spherical geometry (s2) setting unchanged.

expect_search_unchanged <- function(code) {
  before <- search()
  suppressMessages(suppressWarnings(force(code)))
  expect_identical(search(), before)
}

local_s2 <- function(value, env = parent.frame()) {
  old <- suppressMessages(sf::sf_use_s2(value))
  withr::defer(suppressMessages(sf::sf_use_s2(old)), envir = env)
}

coords_country <- data.frame(
  decimalLongitude = c(-63.43333, -67.91667, -41.90000, -46.69778, -14.43333),
  decimalLatitude = c(-17.90000, -14.43333, -13.25000, -13.82444, -67.91667),
  country = c("", "NA", NA, "Brazil", "")
)

coords_inconsistent <- data.frame(
  decimalLongitude = c(-40.6003, -39.6, -64.105),
  decimalLatitude = c(-19.9358, -13.016667, -12.558),
  country = c("Brazil", "Brazil", "Brazil")
)

test_that("bdc_country_from_coordinates() does not attach packages", {
  expect_search_unchanged(
    bdc_country_from_coordinates(data = coords_country)
  )
})

test_that("bdc_coordinates_country_inconsistent() does not attach packages", {
  expect_search_unchanged(
    bdc_coordinates_country_inconsistent(
      data = coords_inconsistent,
      country_name = "Brazil",
      country = "country"
    )
  )
})

test_that("bdc_coordinates_transposed() does not attach packages", {
  x <- data.frame(
    id = 1:4,
    scientificName = c(
      "Rhinella major", "Scinax ruber",
      "Siparuna guianensis", "Psychotria vellosiana"
    ),
    decimalLatitude = c(63.43333, -14.43333, -41.90000, -46.69778),
    decimalLongitude = c(-17.90000, -67.91667, -13.25000, -13.82444),
    country = c("Bolivia", "Bolivia", "Brazil", "Brazil"),
    countryCode = c("BO", "BO", "BR", "BR")
  )
  expect_search_unchanged(
    bdc_coordinates_transposed(data = x, id = "id", save_outputs = FALSE)
  )
})

test_that("bdc_create_report() does not attach packages", {
  x <- data.frame(
    database_id = paste0("test_", 1:5),
    .scientificName_empty = c(TRUE, TRUE, TRUE, FALSE, FALSE),
    .coordinates_empty = c(TRUE, FALSE, FALSE, TRUE, FALSE),
    .coordinates_outOfRange = c(TRUE, TRUE, TRUE, TRUE, TRUE),
    .summary = c(TRUE, FALSE, FALSE, FALSE, FALSE)
  )
  expect_search_unchanged(
    bdc_create_report(data = x, workflow_step = "prefilter")
  )
})

test_that("bdc_create_figures() does not attach packages", {
  local_temp_project()
  x <- data.frame(
    database_id = c("GBIF_01", "GBIF_02", "GBIF_03", "FISH_04", "FISH_05"),
    decimalLatitude = c(-19.93580, -13.01667, -22.34161, -6.75000, -15.15806),
    decimalLongitude = c(-40.60030, -39.60000, -49.61017, -35.63330, -39.52861),
    .coordinates_empty = c(TRUE, TRUE, TRUE, TRUE, TRUE),
    .invalid_basis_of_records = c(TRUE, FALSE, TRUE, FALSE, TRUE),
    .coordinates_outOfRange = c(TRUE, FALSE, TRUE, TRUE, FALSE),
    .summary = c(TRUE, FALSE, TRUE, FALSE, FALSE)
  )
  expect_search_unchanged(
    bdc_create_figures(data = x, workflow_step = "prefilter")
  )
})

for (s2 in c(TRUE, FALSE)) {
  test_that(paste0("bdc_country_from_coordinates() keeps s2 = ", s2), {
    local_s2(s2)
    suppressMessages(suppressWarnings(
      bdc_country_from_coordinates(data = coords_country)
    ))
    expect_identical(sf::sf_use_s2(), s2)
  })

  test_that(paste0("bdc_coordinates_country_inconsistent() keeps s2 = ", s2), {
    local_s2(s2)
    suppressMessages(suppressWarnings(
      bdc_coordinates_country_inconsistent(
        data = coords_inconsistent,
        country_name = "Brazil",
        country = "country"
      )
    ))
    expect_identical(sf::sf_use_s2(), s2)
  })
}

test_that("s2 is restored when a spatial function fails midway", {
  local_s2(TRUE)
  expect_error(
    suppressMessages(bdc_coordinates_country_inconsistent(
      data = coords_inconsistent,
      country_name = "Brazil",
      country = "country",
      dist = "not a distance"
    ))
  )
  expect_identical(sf::sf_use_s2(), TRUE)
})
