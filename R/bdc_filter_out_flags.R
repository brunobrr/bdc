#' Remove columns with the results of data quality tests
#'
#' This function filters out columns containing the results of data quality
#' tests (i.e., columns starting with '.') or other columns specified.
#'
#' @param data data.frame. Containing columns to be removed.
#' @param col_to_remove logical. Which columns should be removed? Default =
#' "all", which means that all columns containing the results of data quality
#' tests are removed.
#'
#' @return A data.frame without columns specified in 'col_to_remove'.
#' @importFrom dplyr select all_of
#' @importFrom tidyselect starts_with
#' @export
#'
#' @examples
#' x <- data.frame(
#'   database_id = c("test_1", "test_2", "test_3", "test_4", "test_5"),
#'    kindom = c("Plantae", "Plantae", "Animalia", "Animalia", "Plantae"),
#'   .bdc_scientificName_empty = c(TRUE, TRUE, TRUE, FALSE, FALSE),
#'   .bdc_coordinates_empty = c(TRUE, FALSE, FALSE, FALSE, FALSE),
#'   .bdc_coordinates_outOfRange = c(TRUE, FALSE, FALSE, FALSE, FALSE),
#'   .summary = c(TRUE, FALSE, FALSE, FALSE, FALSE)
#' )
#'
#' bdc_filter_out_flags(
#'   data = x,
#'   col_to_remove = "all"
#' )
#'
bdc_filter_out_flags <-
  function(data,
           col_to_remove = "all") {
    if (col_to_remove %in% "all") {
      column_names <-
        data %>%
        dplyr::select(tidyselect::starts_with(".")) %>%
        names()

      data <-
        data %>%
        dplyr::select(-dplyr::all_of(column_names))
    } else {
      w <- which(names(data) %in% col_to_remove)

      column_names <- names(data)[w]

      data <-
        data %>%
        dplyr::select(-dplyr::all_of(column_names))
    }

    message(
      "\nbdc_fiter_out_flags:\nThe following columns were removed from the database:\n",
      paste(column_names, collapse = ", ")
    )

    return(data)
  }
