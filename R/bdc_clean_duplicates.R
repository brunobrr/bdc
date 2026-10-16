#' Internal function. Remove and flag duplicated names
#'
#' This function is used to removes and flags scientific names associated to
#' multiple accepted names. In such cases, names are flagged as
#' "multipleAcceptedNames" in column 'notes'. Information on higher taxa (e.g.,
#' kingdom or phylum) can be used to disambiguates names linked to multiple
#' accepted names.
#'
#' @family taxonomy
#' @param data data.frame. Database exported from bdc_query_names_taxadb function.
#' @param rank_name character string. A taxonomic rank to filter the database.
#' Options available are: "kingdom", "phylum", "class", "order", "family", and
#' "genus".
#' @param rank character string. Taxonomic rank name (e.g. "Plantae",
#' "Animalia", "Aves", "Carnivora". Default is NULL.
#'
#' @details Records flagged are "multipleAcceptedNames" are returned as NA.
#'
#' @return A data frame without duplicated names.
#'
#' @importFrom dplyr filter select
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#'
#' }
bdc_clean_duplicates <-
  function(data,
           rank = NULL,
           rank_name = NULL) {
    bdc_check_rank_args(rank, rank_name)

    # Accepted records first; among the other statuses, the first one in
    # alphabetical order is kept (as before).
    data <- data[order(data$taxonomicStatus), ]

    several <- data$input %in% data$input[duplicated(data$input)]

    # A name matching a single record obeys the same rank filter as a name
    # matching several: outside the requested rank, it is not found.
    single <- data[!several, ]
    if (!is.null(rank)) {
      out_of_rank <- !is.na(single[[rank]]) & single[[rank]] != rank_name
      single[out_of_rank, bdc_taxon_cols(single)] <- NA
    }

    # Only names matching several records need a decision.
    if (!any(several)) {
      return(single)
    }

    candidates <- data[several, ]
    resolved <-
      lapply(
        split(candidates, factor(candidates$input, exclude = NULL)),
        bdc_resolve_candidates,
        rank = rank,
        rank_name = rank_name
      )

    dplyr::bind_rows(c(list(single), unname(resolved)))
  }

#' Internal function. Taxonomic columns of a bdc_filter_name() result
#'
#' @param data data.frame. Records returned by bdc_filter_name(), with the
#'   helper columns added by bdc_query_names_taxadb().
#'
#' @return Names of the columns that come from the taxonomic database.
#' @noRd
bdc_taxon_cols <- function(data) {
  helper_cols <- c("sort", "input", "notes", "original_search", "distance")
  setdiff(names(data), helper_cols)
}

#' Internal function. Keep one record for a name matching several records
#'
#' The rank filter is applied to every candidate record first, so the result
#' does not depend on the order of the records in the taxonomic database. A
#' name left with several accepted records is flagged "multipleAccepted" and
#' its taxonomic columns are returned as NA. A synonym linked to more than one
#' accepted name is also flagged "multipleAccepted"; it keeps the values its
#' records share (its name) and the others are returned as NA. A name with no
#' record in the requested rank is returned with its taxonomic columns as NA.
#'
#' @param rows data.frame. Every record matched by one name, accepted first.
#' @param rank,rank_name See bdc_clean_duplicates().
#'
#' @return A one-row data.frame.
#' @noRd
bdc_resolve_candidates <- function(rows, rank = NULL, rank_name = NULL) {
  taxon_cols <- bdc_taxon_cols(rows)

  # The name's first row with every taxonomic column set to NA
  blank_row <- function(rows) {
    rows <- rows[1, ]
    rows[taxon_cols] <- NA
    rows
  }

  if (!is.null(rank)) {
    in_rank <- is.na(rows[[rank]]) | rows[[rank]] == rank_name
    if (!any(in_rank)) {
      return(blank_row(rows))
    }
    rows <- rows[in_rank, ]
  }

  accepted <- rows$taxonomicStatus %in% "accepted"
  if (sum(accepted) > 1) {
    rows <- blank_row(rows)
    rows$notes <- "multipleAccepted"
    return(rows)
  }

  # A synonym linked to more than one accepted name is not replaced: it keeps
  # the values shared by all its records (its name) and is flagged.
  synonyms <- rows$taxonomicStatus %in% "synonym"
  linked <- rows$acceptedNameUsageID[synonyms]
  linked <- unique(linked[!is.na(linked)])
  if (!any(accepted) && length(linked) > 1) {
    synonym_rows <- rows[synonyms, ]
    rows <- synonym_rows[1, ]
    for (col in taxon_cols) {
      if (length(unique(synonym_rows[[col]])) > 1) {
        rows[[col]] <- NA
      }
    }
    rows$notes <- "multipleAccepted"
    return(rows)
  }

  rows[1, ]
}
