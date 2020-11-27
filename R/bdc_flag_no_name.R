#' Flag missing scientific names
#'
#' @description
#' This function add a new column `.no_name` in the returned dataset
#'
#' @param data a data.frame with the default column names: "database_id", "scientificName", "decimalLongitude", "decimalLatitude"
#' 
#' @importFrom dplyr mutate if_else
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data %>%
#'   bdc_flag_no_name()
#' }
bdc_flag_no_name <- function(data) {

  data <-
    data %>%
    dplyr::mutate(.no_name = if_else(is.na(scientificName), TRUE, FALSE))

  return(data)

}
