remotes::install_github("ropensci/rnaturalearthhires")

merged <-
  here::here("data", "temp", "standard_database.qs") %>%
  qread()

wiki_cntr <- bdc_get_wiki_country()
worldmap <- bdc_get_world_map()


x <-  bdc_standardize_country(
      data = merged,
      country = "country",
      country_names_db = wiki_cntr
  )





test_that("bdc_standardize_country standardize country names", {
  expect_equal(bdc_standardize_country(data = merged, country = "country", country_names_db = wiki_cntr), 
               x)
  
  }
)
