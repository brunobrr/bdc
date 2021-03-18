#' Internal function. Filters duplicated names from database query
#'
#' This function is used to flag and remove names with multiple name from database query (i.e., column 'taxonomicStatus').
#'
#' @param data data.frame. Database exported from bdc_get_taxa_taxadb function.
#' @param rank_name character string. A taxonomic rank to filter the database. Options available are: "kingdom", "phylum", "class", "order", "family", and "genus".
#' @param rank character string. Taxonomic rank name (e.g. "Plantae", "Animalia", "Aves", "Carnivora". Default is NULL.
#' @details If database query returns more than one name, this function filters only the accepted ones, or an unique synonym. If there are more than one accepted name, it returns NA and adds flag in column notes.
#' @return A data frame without duplicated names and flags "|check +1 accepted" in the column 'notes'.
#' @noRd
#' @export
#'
#' @examples
#' \dontrun{
#' }
bdc_clean_duplicates <-
  function(data,
           rank = NULL,
           rank_name = NULL) {
    # Filter all names except those flag as "accepted" (e.g.,  heterotypic, homotypic, pro-parte synonyms, and doubtful )
    data <- data[order(data$taxonomicStatus), ]
    data <- data[!(duplicated(data$input) & data$taxonomicStatus != "accepted"), ]

    # Filter out database according to a taxonomic rank
    if (!is.null(rank_name) & !is.null(rank)) {
      valid_duplicates <-
        data[duplicated(data$input) & data$taxonomicStatus == "accepted", ] %>%
        dplyr::filter(., .data[[rank]] == rank_name | is.na(.data[[rank]]))
    } else if (is.null(rank_name) & !is.null(rank)) {
      message("Please, provide both 'rank_name' and 'rank' arguments")
    } else if (!is.null(rank_name) & is.null(rank)) {
      message("Please, provide both 'rank_name' and 'rank' arguments")
    } else {
      valid_duplicates <-
        data[duplicated(data$input) & data$taxonomicStatus == "accepted", ]
    }

    valid_duplicates <- valid_duplicates %>% dplyr::select(scientificName)
    data <- data[!duplicated(data$input), ]
    uni_names <- unique(valid_duplicates$scientificName)

    # Add a flag to names with more than one accepted name found
    if (length(uni_names) > 0) {
      for (i in 1:length(uni_names)) {
        index <- which(data$scientificName == uni_names[i])
        data[index, 2:(ncol(data)-3)] <- NA
        data[index, "notes"] <- "multipleAccepted"
      }
    }

    return(data)
  }
