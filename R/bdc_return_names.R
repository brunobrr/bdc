#' Internal function. Returns the closest matching names for misspelled names
#'
#' This function searches for candidates names for misspelled names within a
#' specified matching distances. Candidates names are extracted for a supplied
#' taxonomic database from 'taxadb' package informed in the function
#' 'bdc_query_names'.
#'
#' @param sci_name character string. Containing unique scientific names without
#' year and authorship information. Default = "sci_name".
#' @param max_distance numeric. The maximum matching distance between supplied
#' and candidates names. . It ranges from 0 to 1, being 1 an indicative of a
#' perfect match. Default = "0.75".
#' @param species_first_letter character string. Contains candidates names  be
#' calculated from sci_name. Default = "species_first_letter".
#' @return A data.frame whose first column is the closest matching name for a misspelled name and the second one a matching distance.
#' @noRd
#'
#' @examples
#' \donttest{
#' bdc_return_names(
#'   "Cebus apela",
#'   max_distance = 0.75,
#'   species_first_letter = c("Cebus apella", "Cebus apellas")
#' )
#' }
bdc_return_names <- function(sci_name, max_distance, species_first_letter) {
  out <- stringdist::stringdist(sci_name, species_first_letter)
  min_dist_name <-
    species_first_letter[out == sort(out, decreasing = FALSE)[1]][1]
  sorted <- sort(c(nchar(sci_name), nchar(min_dist_name)))
  max_dist <- 1 - (min(out) / sorted[2])

  if (max_dist >= max_distance) {
    return(data.frame(
      original = sci_name,
      suggested = min_dist_name,
      distance = round(max_dist, 2)
    ))
  } else {
    return(data.frame(
      original = sci_name,
      suggested = NA,
      distance = round(max_dist, 2)
    ))
  }
}
