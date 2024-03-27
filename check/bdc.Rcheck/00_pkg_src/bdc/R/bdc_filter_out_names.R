#' Filter out records according to their taxonomic status
#'
#' This function is useful for selecting records according to their taxonomic
#' status.  By default, only records with accepted scientific names are
#' returned.
#'
#' @family taxonomy
#' @param data data.frame. Containing the column "notes" with information on the
#' taxonomic status of scientific names.
#' @param col_name character string. The column name containing notes
#' about the taxonomic status of a name. Default = "notes".
#' @param taxonomic_status character string. Taxonomic status of a name. Default
#' = "accepted".
#' @param opposite logical. Should taxonomic status different from those listed
#' in 'taxonomic_status' be returned? Default = FALSE
#'
#' @details By default, only records with accepted scientific names are kept in
#' the database. Such records are listed in the column 'taxonomic_status' as
#' "accepted", "accepted | replaceSynonym",  "accepted | wasMisspelled" or
#' "accepted | wasMisspelled | replaceSynonym". It is also possible to
#' customize the list of taxonomic notes to be kept in the argument
#' 'taxonomic_status'. See 'notes' in the data.frame resulted from the function
#' \code{\link{bdc_create_report}}. If 'opposite' is TRUE, records with notes
#' different from names listed in 'taxonomic_status' are returned.

#' @return A data.frame filtered out according to names listed in
#' 'taxonomic_status'.
#'
#' @importFrom dplyr filter select distinct pull all_of sym
#' @importFrom stringr str_detect
#'
#' @export
#'
#' @examples
#' df_notes <-
#'   data.frame(
#'     notes = c(
#'       "notFound", "accepted", "accepted | replaceSynonym",
#'       "accepted | wasMisspelled",
#'       "accepted | wasMisspelled | replaceSynonym",
#'       "multipleAccepted",
#'       "heterotypic synonym"
#'     )
#'   )
#'
#' bdc_filter_out_names(
#'   data = df_notes,
#'   taxonomic_status = "accepted",
#'   col_name = "notes",
#'   opposite = FALSE
#' )
#' 
bdc_filter_out_names <-
  function(data,
           col_name = "notes",
           taxonomic_status = "accepted",
           opposite = FALSE) {
    notes <- id <- temp <- .data <- . <- NULL

    if (!is.data.frame(data)) {
      stop("data is not a data.frame")
    }

    if (!col_name %in% names(data)) {
      stop(paste0("column ", "'", col_name, "'", "not found"))
    }

    unique_status <-
      data %>%
      dplyr::select(dplyr::all_of(col_name)) %>%
      dplyr::distinct(.) %>%
      dplyr::pull(.)

    if (!all(taxonomic_status %in% unique_status)) {
      stop(paste0(
        "Taxonomic status provided are not present in column",
        " '",
        col_name,
        "'"
      ))
    }

    data$id <- 1:nrow(data)

    df <- NULL
    for (i in 1:length(taxonomic_status)) {
      temp <-
        data %>%
        dplyr::filter(stringr::str_detect(
          !!dplyr::sym(col_name),
          taxonomic_status[i]
        ))
      df <- bind_rows(df, temp)
    }

    df <- df %>% dplyr::distinct(id, .keep_all = T)

    if (opposite) {
      df <-
        data %>%
        dplyr::filter(!id %in% df$id)
    }

    df <- df %>% dplyr::select(-id)
    return(df)
  }
