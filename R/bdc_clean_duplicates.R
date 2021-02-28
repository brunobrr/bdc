#' Filter duplicated names in bdc_get_taxa_taxadb function.
#'
#' Function only to internal use of bdc_get_taxa_taxadb function.
#' 
#' @param data A data.frame created in bdc_get_taxa_taxadb.  
#'
#' @return Return the data.frame without duplicates. 
#' @export
#'
#' @examples
bdc_clean_duplicates <-
  function(data,
           rank = NULL,
           rank_name = NULL) {
    # Filter all names except those flag as "accepted" (e.g.,  misapplied,
    # heterotypic synonym, synonym, homotypic synonym, proparte synonym )
    data <- data[order(data$taxonomicStatus), ]
    data <- data[!(duplicated(data$input) & data$taxonomicStatus != "accepted"), ]
    
    # Filter out database according to a taxonomic rank
    if (!is.null(rank_name) & !is.null(rank)) {
      valid_duplicates <-
        data[duplicated(data$input) & data$taxonomicStatus == "accepted", ] %>%
        dplyr::filter(., .data[[rank]] == rank_name)
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
        data[index, "notes"] <- "|check +1 accepted"
      }
    }
    
    return(data)
  }
