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

test_that("bdc_query_names_taxadb() returns one row per name, in input order", {
  local_fake_taxadb()
  sci_names <- c(
    "Puma concolor", "puma concolor", "PUMA CONCOLOR", "Puma Concolor",
    "", NA, "Felis concolor", "puma concolor", NA, ""
  )

  res <- suppressMessages(
    bdc_query_names_taxadb(sci_names, db = "gbif", suggest_names = FALSE)
  )

  expect_equal(nrow(res), length(sci_names))
  expect_identical(res$original_search, sci_names)
  # Exact matching is case-sensitive: only the sentence-case spellings match.
  expect_equal(
    res$scientificName,
    c(
      "Puma concolor", NA, NA, NA, NA, NA, "Puma concolor", NA, NA, NA
    )
  )
  expect_equal(
    res$notes,
    c(
      "accepted", "notFound", "notFound", "notFound", NA, NA,
      "accepted | replaceSynonym", "notFound", NA, NA
    )
  )
})

test_that("bdc_query_names_taxadb() keeps case variants aligned when suggesting names", {
  local_fake_taxadb()
  sci_names <- c(
    "Puma concolor", "puma concolor", "PUMA CONCOLOR", "Puma Concolor", NA
  )

  res <- suppressMessages(bdc_query_names_taxadb(sci_names, db = "gbif"))

  expect_equal(nrow(res), length(sci_names))
  expect_identical(res$original_search, sci_names)
  expect_equal(res$notes[1], "accepted")
})

# Branches and options ---------------------------------------------------------

test_that("bdc_query_names_taxadb() rejects an unknown provider", {
  local_fake_taxadb()

  expect_error(
    bdc_query_names_taxadb("Puma concolor", db = "not_a_db"),
    "not_a_db provided is not a valid name"
  )
})

test_that("bdc_query_names_taxadb() explains why fb and slb are unavailable", {
  local_fake_taxadb()

  for (db in c("fb", "slb")) {
    expect_error(
      bdc_query_names_taxadb("Puma concolor", db = db),
      paste(db, "database is not available in taxadb version 22.12")
    )
  }
})

test_that("bdc_query_names_taxadb() rejects providers removed from taxadb", {
  local_fake_taxadb()

  for (db in c("iucn", "tpl", "wd")) {
    expect_error(
      bdc_query_names_taxadb("Puma concolor", db = db),
      paste(db, "database is unavailable in taxadb package")
    )
  }
})

test_that("bdc_query_names_taxadb() replaces a synonym with its accepted name", {
  local_fake_taxadb(fake_dwc_taxa(
    taxonID = c("T:1", "T:2"),
    scientificName = c("Aus bus", "Cus dus"),
    taxonRank = "species",
    taxonomicStatus = c("accepted", "synonym"),
    acceptedNameUsageID = "T:1"
  ))

  res <- suppressMessages(bdc_query_names_taxadb("Cus dus", db = "gbif"))

  expect_equal(res$original_search, "Cus dus")
  expect_equal(res$scientificName, "Aus bus")
  expect_equal(res$taxonomicStatus, "accepted")
  expect_equal(res$acceptedNameUsageID, "T:1")
  expect_equal(res$notes, "accepted | replaceSynonym")
})

test_that("bdc_query_names_taxadb() returns the accepted taxonID for a replaced synonym", {
  local_fake_taxadb(fake_dwc_taxa(
    taxonID = c("T:1", "T:2"),
    scientificName = c("Aus bus", "Cus dus"),
    taxonRank = "species",
    taxonomicStatus = c("accepted", "synonym"),
    acceptedNameUsageID = "T:1"
  ))

  res <- suppressMessages(bdc_query_names_taxadb("Cus dus", db = "gbif"))

  expect_equal(res$taxonID, "T:1")
})

test_that("bdc_query_names_taxadb() flags a synonym without accepted name", {
  local_fake_taxadb(fake_dwc_taxa(
    taxonID = c("T:1", "T:2"),
    scientificName = c("Aus bus", "Cus dus"),
    taxonRank = "species",
    taxonomicStatus = c("accepted", "synonym"),
    acceptedNameUsageID = c("T:1", NA)
  ))

  res <- suppressMessages(
    bdc_query_names_taxadb(c("Cus dus", "Aus bus"), db = "gbif")
  )

  # The synonym is kept as found, and only it is flagged.
  expect_equal(res$scientificName, c("Cus dus", "Aus bus"))
  expect_equal(res$taxonomicStatus, c("synonym", "accepted"))
  expect_equal(res$notes, c("synonym | noAcceptedName", "accepted"))
})

test_that("bdc_query_names_taxadb() flags a synonym with several accepted names", {
  local_fake_taxadb(fake_dwc_taxa(
    taxonID = c("T:1", "T:2", "T:3", "T:4"),
    scientificName = c("Aus bus", "Cus dus", "Eus fus", "Eus fus"),
    taxonRank = "species",
    taxonomicStatus = c("accepted", "accepted", "synonym", "synonym"),
    acceptedNameUsageID = c("T:1", "T:2", "T:1", "T:2")
  ))

  res <- suppressMessages(bdc_query_names_taxadb("Eus fus", db = "gbif"))

  expect_equal(res$original_search, "Eus fus")
  expect_match(res$notes, "multipleAccepted", fixed = TRUE)
  expect_false(grepl("replaceSynonym", res$notes, fixed = TRUE))
  # Spec #327: the synonym is not replaced and keeps its name.
  expect_equal(res$notes, "multipleAccepted")
  expect_equal(res$scientificName, "Eus fus")
})

test_that("bdc_query_names_taxadb(replace_synonyms = FALSE) keeps synonyms", {
  local_fake_taxadb(fake_dwc_taxa(
    taxonID = c("T:1", "T:2"),
    scientificName = c("Aus bus", "Cus dus"),
    taxonRank = "species",
    taxonomicStatus = c("accepted", "synonym"),
    acceptedNameUsageID = "T:1"
  ))

  res <- suppressMessages(
    bdc_query_names_taxadb(
      c("Cus dus", "Aus bus"),
      db = "gbif", replace_synonyms = FALSE
    )
  )

  expect_equal(res$scientificName, c("Cus dus", "Aus bus"))
  expect_equal(res$taxonID, c("T:2", "T:1"))
  expect_equal(res$taxonomicStatus, c("synonym", "accepted"))
  expect_equal(res$acceptedNameUsageID, c("T:1", "T:1"))
  expect_equal(res$notes, c("synonym", "accepted"))
})

test_that("bdc_query_names_taxadb(suggest_names = FALSE) does not fix misspellings", {
  local_fake_taxadb()
  taxa_tbl_calls <- 0
  local_mocked_bindings(
    taxa_tbl = function(...) {
      taxa_tbl_calls <<- taxa_tbl_calls + 1
      stop("fuzzy matching ran with suggest_names = FALSE")
    },
    .package = "taxadb"
  )

  res <- suppressMessages(
    bdc_query_names_taxadb(
      c("Puma concolar", "Puma concolor"),
      db = "gbif", suggest_names = FALSE
    )
  )

  expect_equal(taxa_tbl_calls, 0)
  expect_equal(res$notes, c("notFound", "accepted"))
  expect_true(all(is.na(res$suggested_name)))
  expect_equal(res$scientificName, c(NA, "Puma concolor"))
})

# "Casearia" is an accepted genus name in both Animalia and Plantae, the
# example given in the help page.
casearia_rows <- function() {
  fake_dwc_taxa(
    taxonID = c("T:1", "T:2"),
    scientificName = "Casearia",
    taxonRank = "genus",
    taxonomicStatus = "accepted",
    kingdom = c("Animalia", "Plantae")
  )
}

test_that("bdc_query_names_taxadb() flags a name with several accepted records", {
  local_fake_taxadb(casearia_rows())

  res <- suppressMessages(
    bdc_query_names_taxadb(c("Casearia", "Casearia"), db = "gbif")
  )

  expect_equal(res$original_search, c("Casearia", "Casearia"))
  expect_equal(res$notes, c("multipleAccepted", "multipleAccepted"))
  expect_equal(res$scientificName, c(NA_character_, NA_character_))
  expect_equal(res$kingdom, c(NA_character_, NA_character_))
})

test_that("bdc_query_names_taxadb() rank filtering keeps the requested kingdom", {
  local_fake_taxadb(casearia_rows())

  res <- suppressMessages(
    bdc_query_names_taxadb(
      "Casearia",
      db = "gbif", rank = "kingdom", rank_name = "Animalia"
    )
  )

  expect_equal(res$notes, "accepted")
  expect_equal(res$scientificName, "Casearia")
  expect_equal(res$taxonID, "T:1")
  expect_equal(res$kingdom, "Animalia")
})

test_that("bdc_query_names_taxadb() rank filtering ignores database record order", {
  local_fake_taxadb(casearia_rows())

  res <- suppressMessages(
    bdc_query_names_taxadb(
      "Casearia",
      db = "gbif", rank = "kingdom", rank_name = "Plantae"
    )
  )

  expect_equal(res$notes, "accepted")
  expect_equal(res$taxonID, "T:2")
  expect_equal(res$kingdom, "Plantae")
})

test_that("bdc_query_names_taxadb() rank filtering also checks single-record names", {
  # "Cedrela" gives the name suggestion step a Plantae candidate too far from
  # "Casearia" to be suggested.
  local_fake_taxadb(fake_dwc_taxa(
    taxonID = c("T:1", "T:2", "T:3"),
    scientificName = c("Casearia", "Aus bus", "Cedrela"),
    taxonRank = c("genus", "species", "genus"),
    taxonomicStatus = "accepted",
    kingdom = c("Animalia", NA, "Plantae")
  ))

  res <- suppressMessages(
    bdc_query_names_taxadb(
      c("Casearia", "Aus bus"),
      db = "gbif", rank = "kingdom", rank_name = "Plantae"
    )
  )

  # The only record of "Casearia" is an animal, so it is not found; a record
  # without a kingdom is kept, as for names matching several records.
  expect_equal(res$notes, c("notFound", "accepted"))
  expect_equal(res$scientificName, c(NA, "Aus bus"))
  expect_equal(res$taxonID, c(NA, "T:2"))
})

test_that("bdc_query_names_taxadb() reports misspellings sharing a suggestion", {
  local_fake_taxadb()

  expect_message(
    bdc_query_names_taxadb(c("Puma concolar", "Puma concolr"), db = "gbif"),
    "more than one sci_name with the same suggested name"
  )
})

test_that("bdc_query_names_taxadb() suggests one name for two misspellings", {
  local_fake_taxadb()

  res <- suppressMessages(
    bdc_query_names_taxadb(c("Puma concolar", "Puma concolr"), db = "gbif")
  )

  expect_equal(res$original_search, c("Puma concolar", "Puma concolr"))
  expect_equal(res$suggested_name, c("Puma concolor", "Puma concolor"))
  expect_equal(res$scientificName, c("Puma concolor", "Puma concolor"))
  expect_equal(
    res$notes,
    c("accepted | wasMisspelled", "accepted | wasMisspelled")
  )
})

test_that("bdc_query_names_taxadb(export_accepted = TRUE) writes the check file", {
  root <- local_temp_project()
  local_fake_taxadb(casearia_rows())
  path <- file.path(root, "Output", "Check", "02_names_multiple_accepted_names.csv")

  msgs <- capture_messages(
    bdc_query_names_taxadb("Casearia", db = "gbif", export_accepted = TRUE)
  )

  # The message points to the file actually written.
  expect_true(any(grepl(path, msgs, fixed = TRUE)))
  expect_true(file.exists(path))
  written <- readr::read_csv(path, show_col_types = FALSE)
  expect_true(all(c("taxonID", "scientificName", "kingdom") %in% names(written)))
})

test_that("bdc_query_names_taxadb(export_accepted = TRUE) lists every candidate record", {
  root <- local_temp_project()
  local_fake_taxadb(casearia_rows())
  path <- file.path(root, "Output", "Check", "02_names_multiple_accepted_names.csv")

  suppressMessages(
    bdc_query_names_taxadb("Casearia", db = "gbif", export_accepted = TRUE)
  )

  written <- readr::read_csv(path, show_col_types = FALSE)
  expect_setequal(written$taxonID, c("T:1", "T:2"))
  expect_equal(written$scientificName, c("Casearia", "Casearia"))
  expect_setequal(written$kingdom, c("Animalia", "Plantae"))
})
