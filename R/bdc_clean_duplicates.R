#' Title: Filter duplicated names in bdc_get_taxa_taxadb function.
#' Function only to internal use of bdc_get_taxa_taxadb function.
#' 
#' @param data A data.frame created in bdc_get_taxa_taxadb.  
#'
#' @return Return the data.frame without duplicates. 
#' @export
#'
#' @examples
bdc_clean_duplicates <- function(data) {
  data <-
    data %>%
    dplyr::filter(!(duplicated(input) & taxonomicStatus != "accepted"))

  if (!is.null(rank_name) & !is.null(rank)) {
    valid_duplicates <-
      data %>%
      dplyr::filter(duplicated(input) & taxonomicStatus == "accepted") %>%
      dplyr::filter(., .data[[rank]] == rank_name) %>%
      dplyr::mutate(notes = "|check +1 accepted")
  } else if (is.null(rank_name) & !is.null(rank)) {
    message("Please, provide both 'rank_name' and 'rank' arguments")
  } else if (!is.null(rank_name) & is.null(rank)) {
    message("Please, provide both 'rank_name' and 'rank' arguments")
  } else {
    valid_duplicates <-
      data %>%
      dplyr::filter(duplicated(input) & taxonomicStatus == "accepted") %>%
      dplyr::mutate(notes = "|check +1 accepted")
  }

  data.table::fwrite(valid_duplicates, here::here("Output/Check/02_names_multiple_accepted_names.csv"))

  message("\nCheck names with more than one valid name in 'Output/Check/02_names_multiple_accepted_names.csv'\n")

  valid_duplicates <- valid_duplicates %>% dplyr::select(scientificName)

  data <-
    data %>%
    dplyr::filter(!duplicated(input))

  if (length(valid_duplicates[[1]]) > 0) {
    for (i in unique(valid_duplicates$scientificName)) {
      index <- grep(i, data$scientificName)
      data[index, "notes"] <- "|check +1 accepted"
      data$input[index] <- i
    }
  }

  return(data)
}
