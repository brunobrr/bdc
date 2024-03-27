sci_names <- c("Janusia occhioni", "Janusia", "Xilosma ciliatifolium", "Crataeva benthamii", "Oxalis rhombeo ovata", "Cebus apella", "Puma concolar")

test_that("bdc_return_names return the closest name", {
  expect_equal(
    bdc_return_names("Cebus apela", max_distance = 0.75, species_first_letter = sci_names),
    data.frame(original = "Cebus apela", suggested = "Cebus apella", distance = 0.92)
  )

  expect_equal(
    bdc_return_names("Janusia occhini", max_distance = 0.75, species_first_letter = sci_names),
    data.frame(original = "Janusia occhini", suggested = "Janusia occhioni", distance = 0.94)
  )
})

test_that("max_dist >= max_distance", {
  expect_equal(
    bdc_return_names("Cebus apela", max_distance = 0.98, species_first_letter = sci_names),
    data.frame(original = "Cebus apela", suggested = NA, distance = 0.92)
  )
})


test_that("return data frame", {
  expect_equal(
    class(bdc_return_names("Cebus apela", max_distance = 0.75, species_first_letter = sci_names)),
    "data.frame"
  )
})
