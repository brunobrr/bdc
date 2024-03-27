expected <- tibble(data.frame(
  original_search = c("Puma concola", "Cebus apela"),
  sort = integer(2),
  taxonID = character(2),
  scientificName = character(2),
  taxonRank = character(2),
  taxonomicStatus = character(2),
  acceptedNameUsageID = character(2),
  kingdom = character(2),
  phylum = character(2),
  class = character(2),
  order = character(2),
  family = character(2),
  genus = character(2),
  specificEpithet = character(2),
  infraspecificEpithet = character(2),
  parentNameUsageID = character(2),
  originalNameUsageID = character(2),
  scientificNameAuthorship = character(2),
  vernacularName = character(2),
  input = character(2),
  notes = character(2),
  distance = numeric(2)
))

expected$original_search <- c("Puma concola", "Puma concolo")

expected$input <- c("Puma concolor", "Puma concolor")

expected$taxonomicStatus <- c("accepted", "accepted")

expected$class <- c("Mammalia", "Mammalia")

expected$sort <- as.integer(c(1, 2))

for (i in 3:ncol(expected)) {
  expected[, i] <- as.character(c(NA, NA))
}

expected$distance <- as.numeric(c(0, 0))

expected$notes <- c("NA", "NA")

test_that("removing duplcated names from bdc_query_names_taxadb", {
  rows_size <- dim(bdc_clean_duplicates(expected))[1]

  testthat::expect_equal(rows_size, 1)
})

test_that("testing rank", {
  rows_size <- dim(bdc_clean_duplicates(expected, rank = "class", rank_name = "Mammalia"))[1]

  testthat::expect_equal(rows_size, 1)
})

test_that("rank_name without rank", {
  message <- testthat::capture_message(suppressWarnings(bdc_clean_duplicates(expected, rank_name = "Mammalia")))

  testthat::expect_equal(message$message, "Please, provide both 'rank_name' and 'rank' arguments\n")
})

test_that("rank without rank_name", {
  message <- testthat::capture_message(suppressWarnings(bdc_clean_duplicates(expected, rank = "class")))

  testthat::expect_equal(message$message, "Please, provide both 'rank_name' and 'rank' arguments\n")
})
