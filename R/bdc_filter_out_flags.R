#' Filter out rows based on flags assigned as FALSE
#'
#' @description
#' This functions filter out rows based on any flag column assigned as FALSE
#'
#' @param data a data.frame with flags created by functions bdc_flag_*
#' 
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
bdc_filter_out_flags <- function(data) {

  column_names <-
    data %>%
    dplyr::select(starts_with(".")) %>%
    names()

  data <-
    data %>%
    dplyr::filter_at(vars(column_names), all_vars(. == TRUE))

  message("Filtering out columns: ", paste(column_names, collapse = ", "))

  return(data)

}
