skip_on_ci()
skip_on_cran()

wiki_cntr <- system.file("extdata/countries_names/country_names.txt", package = "bdc") %>%
  readr::read_delim(delim = "\t", show_col_types = FALSE) 

worldmap <- bdc_get_world_map()

data <- data.frame(country = c("brezil", "USA", "Bolibia", "vietnam"))

x <- bdc_standardize_country(
  data = data,
  country = "country",
  country_names_db = wiki_cntr
)


test_that("bdc_standardize_country standardize country names", {
  expect_equal(
    x$cntr_suggested,
    c("Bolivia", "Brazil", "United States of America",   "Vietnam")
  )
})

test_that("bdc_standardize_country original names", {
  expect_equal(
    x$cntr_original,
    c("Bolibia", "brezil", "USA", "vietnam")
  )
})

test_that("bdc_standardize_country iso", {
  expect_equal(
    x$cntr_iso2c,
    c("BO", "BR", "US", "VN")
  )
})

