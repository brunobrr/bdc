## skip("dont run")
skip_on_cran()
skip_if_not_installed("curl")

sci_names <-
  c(
    "Janusia occhioni",
    "Janusia",
    "Xilosma ciliatifolium",
    "Crataeva benthamii",
    "Oxalis rhombeo ovata",
    "Cebus apella",
    "Puma concolar"
  )

res <-
  suppressWarnings(
    bdc_suggest_names_taxadb(
      sci_name = sci_names,
      max_distance = 0.75,
      provider = "gbif",
    )
  )

test_that("gbif - bdc_suggest_names_taxadb suggest valid names", {
  expect_equal(res, data.frame(
    original = sci_names, 
    suggested = c(
      "Janusia occhionii",
      "Janusia",
      "Xylosma ciliatifolium",
      "Crateva benthamii",
      "Oxalis rhombeoovata",
      "Cebus apella",
      "Puma concolor"
    ),
    distance = c(0.94, 1.00, 0.95, 0.94, 0.95, 1.00, 0.92)
  ))
})

res <-
  suppressWarnings(
    bdc_suggest_names_taxadb(
      sci_name = sci_names,
      max_distance = 0.75,
      provider = "itis",
      db_version = 2022,
      parallel = FALSE
    )
  )

test_that("itis - bdc_suggest_names_taxadb suggest valid names", {
  expect_equal(res, data.frame(
    original = sci_names, suggested = c(
      NA,
      "Janusia",
      NA,
      NA,
      NA,
      "Cebus apella",
      "Puma concolor"
    ),
    distance = c(0.62, 1.00, 0.67, 0.67, 0.55, 1.00, 0.92)
  ))
})

res <-
  suppressWarnings(
    bdc_suggest_names_taxadb(
      sci_name = sci_names,
      max_distance = 0.75,
      provider = "ncbi",
      parallel = FALSE
    )
  )

test_that("ncbi - bdc_suggest_names_taxadb suggest valid names", {
  expect_equal(res, data.frame(
    original = sci_names, suggested = c(
      NA,
      "Jansia",
      NA,
      NA,
      "Oxalis rhombeo ovata",
      "Cebus apella",
      "Puma concolor"
    ),
    distance = c(0.63, 0.86, 0.67, 0.68, 1.00, 1.00, 0.92)
  ))
})

res <-
  suppressWarnings(
    bdc_suggest_names_taxadb(
      sci_name = sci_names,
      max_distance = 0.75,
      provider = "col",
      parallel = FALSE
    )
  )


test_that("col - bdc_suggest_names_taxadb suggest valid names", {
  expect_equal(res, data.frame(
    original = sci_names, suggested = c(
      "Janusia occhionii",
      NA,
      "Xylosma ciliatifolium",
      "Crateva benthamii",
      "Oxalis rhombeoovata",
      "Cebus apella",
      "Puma concolor"
    ),
    distance = c(0.94, 0.43, 0.95, 0.94, 0.95, 1, 0.92)
  ))
})

res <-
  suppressWarnings(
    bdc_suggest_names_taxadb(
      sci_name = sci_names,
      max_distance = 0.75,
      provider = "ott",
      db_version = 2021,
      parallel = FALSE
    )
  )

res$distance
test_that("ott - bdc_suggest_names_taxadb suggest valid names", {
  expect_equal(res, data.frame(
    original = sci_names, suggested = c(
      "Janusia occhionii",
      "Janusia",
      "Xylosma ciliatifolium",
      "Crateva benthamii",
      "Oxalis rhombeoovata",
      "Cebus apella",
      "Puma concolor"
    ),
    distance = c(0.94, 1, 0.95, 0.94, 0.95, 1, 0.92)
  ))
})

res <-
  suppressWarnings(
    bdc_suggest_names_taxadb(
      sci_name = sci_names,
      max_distance = 0.75,
      provider = "itis",
      db_version = 2022,
      parallel = TRUE
    )
  )

test_that("testing parallelization", {
  expect_equal(res, data.frame(
    original = sci_names, suggested = c(
      NA,
      "Janusia",
      NA,
      NA,
      NA,
      "Cebus apella",
      "Puma concolor"
    ),
    distance = c(0.62, 1.00, 0.67, 0.67, 0.55, 1.00, 0.92)
  ))
})

res <- suppressWarnings(
  bdc_suggest_names_taxadb(
    sci_name = sci_names,
    rank_name = "Plantae",
    rank = "kingdom",
    max_distance = 0.75,
    provider = "itis",
    db_version = 2022,
    parallel = FALSE
  )
)

test_that("testing ranks", {
  expect_equal(res, data.frame(
    original = sci_names, suggested = c(
      NA,
      "Janusia",
      NA,
      NA,
      NA,
      NA,
      NA
    ),
    distance = c(0.56, 1.00, 0.67, 0.67, 0.55, 0.67, 0.64)
  ))
})

res <-
  testthat::capture_error(suppressWarnings(
    bdc_suggest_names_taxadb(
      sci_name = sci_names,
      rank_name = "plantae",
      rank = NULL,
      max_distance = 0.75,
      provider = "ott",
      db_version = 2021,
      parallel = FALSE
    )
  ))

test_that("testing null rank", {
  expect_equal(res$message,
               "Please, provide both 'rank_name' and 'rank' arguments")
})

res <-
  testthat::capture_error(suppressWarnings(
    bdc_suggest_names_taxadb(
      sci_name = sci_names,
      rank = "kingdom",
      max_distance = 0.75,
      provider = "ott",
      db_version = 2021,
      parallel = FALSE
    )
  ))

test_that("testing null rank_name", {
  expect_equal(res$message,
               "Please, provide both 'rank_name' and 'rank' arguments")
})
