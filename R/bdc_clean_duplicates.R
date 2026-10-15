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
    if (is.null(rank_name) != is.null(rank)) {
      stop("Please, provide both 'rank' and 'rank_name' arguments, or neither")
    }

    # Accepted records first; among the other statuses, the first one in
    # alphabetical order is kept (as before).
    data <- data[order(data$taxonomicStatus), ]

    # Only names matching several records need a decision.
    several <- data$input %in% data$input[duplicated(data$input)]
    if (!any(several)) {
      return(data)
    }

    candidates <- data[several, ]
    resolved <-
      lapply(
        split(candidates, factor(candidates$input, exclude = NULL)),
        bdc_resolve_candidates,
        rank = rank,
        rank_name = rank_name
      )

    dplyr::bind_rows(c(list(data[!several, ]), unname(resolved)))
  }

#' Internal function. Keep one record for a name matching several records
#'
#' The rank filter is applied to every candidate record first, so the result
#' does not depend on the order of the records in the taxonomic database. A
#' name left with several accepted records is flagged "multipleAccepted" and
#' its taxonomic columns are returned as NA. A name with no record in the
#' requested rank is returned with its taxonomic columns as NA.
#'
#' @param rows data.frame. Every record matched by one name, accepted first.
#' @param rank,rank_name See bdc_clean_duplicates().
#'
#' @return A one-row data.frame.
#' @noRd
bdc_resolve_candidates <- function(rows, rank = NULL, rank_name = NULL) {
  helper_cols <- c("sort", "input", "notes", "original_search", "distance")
  taxon_cols <- setdiff(names(rows), helper_cols)

  if (!is.null(rank)) {
    in_rank <- is.na(rows[[rank]]) | rows[[rank]] == rank_name
    if (!any(in_rank)) {
      rows <- rows[1, ]
      rows[taxon_cols] <- NA
      return(rows)
    }
    rows <- rows[in_rank, ]
  }

  if (sum(rows$taxonomicStatus %in% "accepted") > 1) {
    rows <- rows[1, ]
    rows[taxon_cols] <- NA
    rows$notes <- "multipleAccepted"
    return(rows)
  }

  rows[1, ]
}
