#' Title
#'
#' @param data 
#' @param notes 
#' @param opposite 
#'
#' @return
#' @export
#'
#' @examples
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