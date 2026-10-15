# Fake taxadb boundary ---------------------------------------------------------
#
# Name harmonization reaches taxadb through four doors: the package's exact-name
# match (`bdc_filter_name()`), its id match (`bdc_filter_id()`), and taxadb's
# `td_create()` and `taxa_tbl()` (the latter used by fuzzy matching).
# `local_fake_taxadb()` replaces all of them with an in-memory table of
# Darwin Core rows, so tests run offline in milliseconds. `td_connect()` is
# replaced by a tripwire: reaching it means a real database call slipped past
# the fake.

# Columns, in order, of a taxadb 22.12 table with the "dwc" schema.
dwc_columns <- c(
  "taxonID", "scientificName", "taxonRank", "taxonomicStatus",
  "acceptedNameUsageID", "kingdom", "phylum", "class", "order", "family",
  "genus", "specificEpithet", "infraspecificEpithet", "parentNameUsageID",
  "originalNameUsageID", "scientificNameAuthorship", "vernacularName"
)

#' Build fake taxadb rows in the Darwin Core shape
#'
#' Takes columns as named vectors (recycled like `tibble::tibble()`), fills
#' every other Darwin Core column with `NA_character_` and returns them in
#' taxadb's column order. When `acceptedNameUsageID` is not supplied, accepted
#' rows point to their own `taxonID`, as in taxadb.
#'
#' @examples
#' fake_dwc_taxa(
#'   taxonID = c("T:1", "T:2"),
#'   scientificName = c("Puma concolor", "Felis concolor"),
#'   taxonomicStatus = c("accepted", "synonym"),
#'   acceptedNameUsageID = c("T:1", "T:1")
#' )
#' @noRd
fake_dwc_taxa <- function(...) {
  rows <- tibble::tibble(...)
  unknown <- setdiff(names(rows), dwc_columns)
  if (length(unknown) > 0) {
    stop("Not a Darwin Core column: ", paste(unknown, collapse = ", "))
  }
  if (!"acceptedNameUsageID" %in% names(rows) &&
    all(c("taxonID", "taxonomicStatus") %in% names(rows))) {
    rows$acceptedNameUsageID <-
      ifelse(rows$taxonomicStatus == "accepted", rows$taxonID, NA_character_)
  }
  for (col in setdiff(dwc_columns, names(rows))) {
    rows[[col]] <- rep(NA_character_, nrow(rows))
  }
  rows[dwc_columns]
}

#' Default fake taxadb content
#'
#' Mirrors the gbif 22.12 records for "Puma concolor" (accepted) and its
#' synonym "Felis concolor".
#' @noRd
fake_taxadb_rows <- function() {
  fake_dwc_taxa(
    taxonID = c("GBIF:2435099", "GBIF:2435104"),
    scientificName = c("Puma concolor", "Felis concolor"),
    taxonRank = "species",
    taxonomicStatus = c("accepted", "synonym"),
    acceptedNameUsageID = "GBIF:2435099",
    kingdom = "Animalia",
    phylum = "Chordata",
    class = "Mammalia",
    order = "Carnivora",
    family = "Felidae",
    genus = "Puma",
    specificEpithet = "concolor",
    parentNameUsageID = "2435098",
    originalNameUsageID = c("2435104", NA),
    scientificNameAuthorship = c("(Linnaeus, 1771)", "Linnaeus, 1771"),
    vernacularName = c("Red Tiger", "puma")
  )
}

#' Replace the taxadb boundary with in-memory rows for the calling test
#'
#' Exact-name and id matching behave like the real helpers: a case-sensitive
#' right join on `scientificName` (or `taxonID`) that keeps every query, adds
#' `input` and `sort`, and returns one row per matching record (so duplicated
#' names in `rows` yield several matches). `taxa_tbl()` returns `rows` whatever
#' the provider. Mocks are removed when `.local_envir` exits.
#'
#' @param rows Darwin Core rows, usually built with `fake_dwc_taxa()`.
#' @param .local_envir Environment whose exit removes the mocks.
#' @return `rows`, invisibly.
#' @noRd
local_fake_taxadb <- function(rows = fake_taxadb_rows(),
                              .local_envir = parent.frame()) {
  match_on <- function(keys, column) {
    db <- dplyr::mutate(rows, input = .data[[column]])
    queries <- tibble::tibble(input = keys, sort = seq_along(keys))
    dplyr::right_join(db, queries, by = "input", na_matches = "never")
  }

  testthat::local_mocked_bindings(
    bdc_filter_name = function(sci_name, db, db_version = "22.12") {
      out <- match_on(sci_name, "scientificName")
      dplyr::relocate(out, "sort", .before = "taxonID")
    },
    bdc_filter_id = function(id, db, db_version = "22.12") {
      out <- match_on(id, "taxonID")
      dplyr::relocate(out, "sort", .after = "taxonID")
    },
    .env = .local_envir
  )

  testthat::local_mocked_bindings(
    td_create = function(...) invisible(TRUE),
    taxa_tbl = function(...) rows,
    td_connect = function(...) {
      stop("fake taxadb: td_connect() reached; a real database call escaped")
    },
    .package = "taxadb",
    .env = .local_envir
  )

  invisible(rows)
}
