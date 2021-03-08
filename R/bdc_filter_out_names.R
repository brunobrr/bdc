#' Removes records with taxonomic issues
#'
#' This function filters out records whose scientific names contain a issue,
#' including names not found ('notFound'), with multiple (multipleAccepted) or
#' none (noAcceptedName) accepted name found.
#' 
#' @param data data frame containing a column 'notes'.
#' @param notes character string. A column containing notes on taxonomic standardization process. Default = "notes".
#' @param opposite logical. Should records with not taxonomic issue be returned.
#' Default = FALSE.
#' @details The function filters out records whose notes column flags taxonomic issues such as 'noFound', 'multipleAccepted' and 'noAcceptedName'.
#' @return A data frame without records with taxonomic issues. If opposite is TRUE, only records with taxonomic issues are returned. 
#' @export
#'
#' @examples
#' 
bdc_filter_out_names <-
  function(data,
           notes = c(
             "not_found", "more_one_accepted",
             "no_accepted", "taxo_uncer"
           ),
           opposite = FALSE) {
    data <-
      data %>%
      dplyr::mutate(temp_id = 1:nrow(data))

    posi <- NULL

    if (any(notes == "not_found")) {
      x <- which(data$notes == "not found")
      posi <- c(posi, x)
    }

    if (any(notes == "more_one_accepted")) {
      x <-
        stringr::str_which(data$notes, regex("1 accepted"))
      posi <- c(posi, x)
    }

    if (any(notes == "no_accepted")) {
      x <-
        stringr::str_which(data$notes, regex("check no accepted name"))
      posi <- c(posi, x)
    }

    if (any(notes == "taxo_uncer")) {
      x <- which(data$.uncer_terms == FALSE)
      posi <- c(posi, x)
    }

    res_temp <- data[posi, ] %>% dplyr::distinct(temp_id, .keep_all = T)

    if (opposite == FALSE) {
      res <-
        res_temp %>%
        dplyr::select(-temp_id)
    } else {
      res <-
        data %>%
        dplyr::filter(!temp_id %in% res_temp$temp_id) %>%
        dplyr::select(-temp_id)
    }
    return(res)
  }