# Offline tests for bdc_query_names_taxadb(): taxadb is replaced by the fake
# boundary in helper-taxadb.R, so these run without network or downloads.
# The network-backed integration tests live in test-bdc_query_names_taxadb.R.

test_that("bdc_query_names_taxadb() harmonizes sentence-case names offline", {
  local_fake_taxadb()
  sci_names <- c("Puma concolor", "Felis concolor", "Puma concolor")

  res <- suppressMessages(bdc_query_names_taxadb(sci_names, db = "gbif"))

  # Expected values match the real gbif 22.12 result for these names.
  expect_equal(nrow(res), 3)
  expect_equal(res$original_search, sci_names)
  expect_equal(res$scientificName, rep("Puma concolor", 3))
  expect_equal(res$taxonomicStatus, rep("accepted", 3))
  expect_equal(res$acceptedNameUsageID, rep("GBIF:2435099", 3))
  expect_equal(
    res$notes,
    c("accepted", "accepted | replaceSynonym", "accepted")
  )
  expect_equal(
    names(res),
    c(
      "original_search", "suggested_name", "distance", "notes", "taxonID",
      "scientificName", "taxonRank", "taxonomicStatus", "acceptedNameUsageID",
      "kingdom", "phylum", "class", "order", "family", "genus",
      "specificEpithet", "infraspecificEpithet", "parentNameUsageID",
      "originalNameUsageID", "scientificNameAuthorship", "vernacularName"
    )
  )
})

test_that("bdc_query_names_taxadb(parallel = FALSE) starts no worker processes", {
  local_fake_taxadb()
  clusters_made <- 0
  local_mocked_bindings(
    makeCluster = function(...) {
      clusters_made <<- clusters_made + 1
      stop("makeCluster() must not be called when parallel = FALSE")
    },
    .package = "parallel"
  )

  # "Puma concolar" is misspelled, so it reaches fuzzy matching.
  res <- suppressMessages(
    bdc_query_names_taxadb("Puma concolar", db = "gbif", parallel = FALSE)
  )

  expect_equal(clusters_made, 0)
  expect_equal(res$scientificName, "Puma concolor")
  expect_equal(res$notes, "accepted | wasMisspelled")
})

test_that("bdc_query_names_taxadb(parallel = TRUE) stops the cluster on error", {
  local_fake_taxadb()
  fake_cluster <- structure(list(), class = "fake_cluster")
  stopped <- list()
  local_mocked_bindings(
    makeCluster = function(...) fake_cluster,
    stopCluster = function(cl) stopped[[length(stopped) + 1]] <<- cl,
    .package = "parallel"
  )
  # Fail right after the cluster exists, as a failing worker would.
  local_mocked_bindings(
    registerDoParallel = function(...) stop("induced matching failure"),
    .package = "doParallel"
  )

  expect_error(
    suppressMessages(
      bdc_query_names_taxadb("Puma concolar", db = "gbif", parallel = TRUE)
    ),
    "induced matching failure"
  )
  expect_equal(stopped, list(fake_cluster))
})

test_that("bdc_query_names_taxadb() needs both rank and rank_name or neither", {
  local_fake_taxadb()
  # Any database access before validation is a failure.
  local_mocked_bindings(
    td_create = function(...) stop("database accessed before validation"),
    .package = "taxadb"
  )
  local_mocked_bindings(
    bdc_filter_name = function(...) stop("database accessed before validation")
  )
  sci_names <- c("Puma concolor", "Felis concolor")

  only_rank <- expect_error(
    bdc_query_names_taxadb(sci_names, db = "gbif", rank = "kingdom")
  )
  only_rank_name <- expect_error(
    bdc_query_names_taxadb(sci_names, db = "gbif", rank_name = "Animalia")
  )

  for (err in list(only_rank, only_rank_name)) {
    expect_match(conditionMessage(err), "'rank'", fixed = TRUE)
    expect_match(conditionMessage(err), "'rank_name'", fixed = TRUE)
  }
})

test_that("bdc_query_names_taxadb() fuzzy matching reads the database once", {
  rows <- local_fake_taxadb()
  taxa_tbl_calls <- 0
  local_mocked_bindings(
    taxa_tbl = function(...) {
      taxa_tbl_calls <<- taxa_tbl_calls + 1
      rows
    },
    .package = "taxadb"
  )

  suppressMessages(bdc_query_names_taxadb("Puma concolar", db = "gbif"))

  expect_equal(taxa_tbl_calls, 1)
})

test_that("bdc_query_names_taxadb() pins taxadb version 22.12 for every provider", {
  local_fake_taxadb()
  versions <- character()
  local_mocked_bindings(
    td_create = function(provider, schema, version, ...) {
      versions[[provider]] <<- version
      invisible(TRUE)
    },
    .package = "taxadb"
  )
  providers <- c("itis", "ncbi", "col", "gbif", "ott")

  for (db in providers) {
    suppressMessages(bdc_query_names_taxadb("Puma concolor", db = db))
  }

  expect_equal(versions, setNames(rep("22.12", 5), providers))
})
