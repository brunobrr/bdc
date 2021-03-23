#' Filter out records according to their taxonomic status
#'
#' This function is useful for selecting records according to their taxonomic
#' status.  By default, only records with accepted scientific names are
#' returned.
#'
#' @param data data.frame. Containing a column with information on the taxonomic
#' status of scientific names.
#' @param taxonomic_notes character string. The column name containing notes on
#' the taxonomic status of a name. Default = "notes".
#' @param opposite logical. Should taxonomic notes different from those listed
#' in 'taxonomic_notes' be returned? Default = FALSE
#'
#' @details By default, only records with accepted scientific names are kept in
#' the database. Such records are listed in the column 'taxonomic_notes' as
#' "accepted", "accepted | replaceSynonym",  "accepted | wasMisspelled" or
#' "accepted | wasMisspelled | replaceSynonym". It is also possible to
#' customize the list of taxonomic notes to be kept in the argument
#' 'taxonomic_notes'. See 'notes' in the data.frame resulted from the function
#' \code{\link{bdc_create_report}}. If 'opposite' is TRUE, records with notes
#' different from names listed in 'taxonomic_notes' are returned.

#' @return A data.frame filtered out according to names listed in
#' 'taxonomic_notes'.
#' 
#' @importFrom dplyr filter select
#' @importFrom stringr str_detect
#' 
#' @export
#'
#' @examples
#' 
bdc_filter_out_names <-
  function(data,
           taxonomic_notes = "accepted", 
           opposite = FALSE) {
    data$id <- 1:nrow(data)
    
    if (!"notes" %in% names(data)) {
      message("column 'notes' not found")
    }

    unique_notes <- unique(data$notes)
    if (!taxonomic_notes %in% unique_notes) {
      message("Names provided are not present in column 'notes'")
    }

    if (all(taxonomic_notes == "accepted")) {
      df <-
        data %>%
        dplyr::filter(stringr::str_detect(notes, "accepted"))
    } else {
      df <- data %>% dplyr::filter(notes %in% taxonomic_notes)
    }
    
    if (opposite == TRUE) {
      df <-
        data %>%
        dplyr::filter(!id %in% df$id)
    }
      
    df <- df %>% dplyr::select(-id)
    return(df)
  }