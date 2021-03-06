#' Removes columns with the results of data quality tests
#'
#' This functions is used to filter out column containing the results of data
#' quality tests (i.e., column starting with '.') or other columns specified.
#'
#' @param data data.frame. Containing columns to be removed
#' @param col_to_remove logical. Which columns should be removed? Default =
#' "all", which means that all columns containing the results of data quality tests
#' are removed.
#' @details
#' @export A data.frame without columns specified in 'col_to_remove'.
#'
#' @examples
#' \dontrun{
#' data %>%
#'   bdc_flag_transposed_xy() %>%
#'   bdc_filter_out_flags()
#' }
bdc_filter_out_flags <- function(data, col_to_remove = "all") {
  if (col_to_remove %in% "all") {
    column_names <-
      data %>%
      dplyr::select(starts_with(".")) %>%
      names()
    
    data <-
      data %>%
      dplyr::select(-all_of(column_names))
  } else {
    w <- which(names(data) %in% col_to_remove)
    
    column_names <- names(data)[w]
    
    data <-
      data %>%
      dplyr::select(-all_of(column_names))
  }
  
  message(
    "\nbdc_fiter_out_flags:\nThe following columns were removed from the database:\n",
    paste(column_names, collapse = ", ")
  )
  
  return(data)
  
}