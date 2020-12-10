context("geographic outlier")

df <-
  tibble::tribble(
                   ~species, ~longitude,  ~latitude,
    "Abarema cochliacarpos", -65.031271, -35.276203,  # Argentina
      "Abarema langsdorfii", -51.980775, -12.936253,  # Brasil
      "Abarema filamentosa", -72.461249, -72.461249,  # Colômbia
          "Abarema jupunba", -15.279004,  -8.204429   # Oceano Atlântico
    )

df_geo_out <-
  bdc_geographic_outlier(
    x = df,
    species = "species",
    longitude = "longitude",
    latitude = "latitude"
  )

test_that("bdc_geographic_outlier flag an non outlier correctly", {

  notout <-
    df_geo_out %>%
    filter(species == "Abarema langsdorfii")

  expect_equal(pull(notout, .geographic_outlier), FALSE)

})

test_that("bdc_geographic_outlier flag an outlier correctly", {

  out <-
    df_geo_out %>%
    filter(species == "Abarema jupunba")

  expect_equal(pull(out, .geographic_outlier), TRUE)

})
