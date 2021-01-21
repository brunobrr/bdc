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
bdc_filter_out_flags <- function(data, rem_summary = F) {

  column_names <-
    data %>%
    dplyr::select(starts_with(".")) %>%
    names()
  
  if (rem_summary == FALSE){
    w <- which(column_names == ".summary")
    column_names <- column_names[-w]
    
    data <-
      data %>%
      dplyr::select(-column_names)
    
  } else{
    data <-
      data %>%
      dplyr::select(-column_names)
  }
  
  message("\nbdc_fiter_out_flags:\nThe following columns were removed from the database:\n", paste(column_names, collapse = ","))

  return(data)

}
