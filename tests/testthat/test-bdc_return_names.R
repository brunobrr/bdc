

sci_names <- c("Janusia occhioni", "Janusia", "Xilosma ciliatifolium",  "Crataeva benthamii", "Oxalis rhombeo ovata", "Cebus apella", "Puma concolar") 

test_that("bdc_return_names return the closest name", {
  expect_equal(bdc_return_names("Cebus apela", max.distance = 0.75, species.first.letter = sci_names), data.frame(suggested = "Cebus apella", distance = 0.92))
  expect_equal(bdc_return_names("Janusia occhini", max.distance = 0.75, species.first.letter = sci_names), data.frame(suggested = "Janusia occhioni", distance = 0.94))
  
}
)
