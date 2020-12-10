#' @title Flag records missing scientific name
#'
#' @description
#' This function add a new column `.missing_name` in the returned dataset
#'
#' @param data a data.frame containing a column of species names: 
#' 
#'
#' @export
#'
#' @examples
bdc_flag_missing_names <- function(data, sci_name = "scientificName") {
  
  data <-
    data %>%
    pull(sci_name) %>%
    trimws(.) %>% 
    ifelse(. == ""|. == "NA", NA, .)
  
  .missing_name <- ifelse(is.na(data) == FALSE, TRUE, FALSE)
  
  return(.missing_name)
  
}
