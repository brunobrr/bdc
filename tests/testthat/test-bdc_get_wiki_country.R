context("wiki country names")

country_names <- bdc_get_wiki_country()

test_that("bdc_get_wiki_country can read the wiki_country_names.txt", {

  country_names

  expect_equal(names(country_names), c("english_name", "names_in_different_languages"))

})
