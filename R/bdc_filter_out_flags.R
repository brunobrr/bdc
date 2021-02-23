#' Filter out rows based on flags assigned as FALSE
#'
#' @description
#' This functions filter out rows based on any flag column assigned as FALSE
#'
#' @param data data.frame with flags created by functions bdc_flag_*
#' @param logical. Should the column .summary be removed?
#' @importFrom dplyr select filter_at
#'
#' @export
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