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
    . <- .data <- scientificName <- NULL
    # Filter all names except those flag as "accepted" (e.g.,  heterotypic,
    # homotypic, pro-parte synonyms, and doubtful)
    data <- data[order(data$taxonomicStatus),]
    data <-
      data[!(duplicated(data$input) &
               data$taxonomicStatus != "accepted"),]
    
    # Filter out database according to a taxonomic rank
    if (!is.null(rank_name) & !is.null(rank)) {
      valid_duplicates <-
        data[duplicated(data$input) &
               data$taxonomicStatus == "accepted",] %>%
        dplyr::filter(., .data[[rank]] == rank_name |
                        is.na(.data[[rank]]))
    } else if (is.null(rank_name) & !is.null(rank)) {
      message("Please, provide both 'rank_name' and 'rank' arguments")
    } else if (!is.null(rank_name) & is.null(rank)) {
      message("Please, provide both 'rank_name' and 'rank' arguments")
    } else {
      valid_duplicates <-
        data[duplicated(data$input) &
               data$taxonomicStatus == "accepted",]
    }
    
    valid_duplicates <- 
      valid_duplicates %>%
      dplyr::select(scientificName)
    
    data <- data[!duplicated(data$input),]
    uni_names <- unique(valid_duplicates$scientificName)
    
    # Add a flag to names with more than one accepted name found
    if (length(uni_names) > 0) {
      for (i in 1:length(uni_names)) {
        index <- which(data$scientificName == uni_names[i])
        data[index, 2:(ncol(data) - 4)] <- NA
        data[index, "notes"] <- "multipleAccepted"
      }
    }
    
    return(data)
  }
