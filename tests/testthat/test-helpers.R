# Tests for the shared fixtures in helper-*.R: they must leave no trace.

test_that("local_temp_project() restores the working directory and here root", {
  wd_before <- getwd()
  here_before <- here::here()

  root <- local({
    root <- local_temp_project(subdir = "a/b")
    expect_equal(normalizePath(getwd()), normalizePath(file.path(root, "a/b")))
    expect_equal(here::here("Output"), file.path(root, "Output"))
    expect_true(file.exists(file.path(root, ".here")))
    root
  })

  expect_equal(getwd(), wd_before)
  expect_equal(here::here(), here_before)
  expect_false(dir.exists(root))
})

test_that("local_fake_taxadb() removes its mocks on exit", {
  real_filter_name <- bdc_filter_name
  real_td_create <- taxadb::td_create

  local({
    local_fake_taxadb()
    expect_false(identical(bdc_filter_name, real_filter_name))
    expect_false(identical(taxadb::td_create, real_td_create))
  })

  expect_identical(bdc_filter_name, real_filter_name)
  expect_identical(taxadb::td_create, real_td_create)
})

test_that("local_fake_taxadb() serves custom rows to bdc_query_names_taxadb()", {
  local_fake_taxadb(fake_dwc_taxa(
    taxonID = "T:1",
    scientificName = "Aus bus",
    taxonomicStatus = "accepted",
    class = "Insecta"
  ))

  res <- suppressMessages(
    bdc_query_names_taxadb(c("Aus bus", "Cus dus"), suggest_names = FALSE)
  )

  expect_equal(res$scientificName, c("Aus bus", NA))
  expect_equal(res$class, c("Insecta", NA))
  expect_equal(res$notes, c("accepted", "notFound"))
})
