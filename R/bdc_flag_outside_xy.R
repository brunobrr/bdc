#' Flag coordinates outside the world coordinates
#'
#' @description
#' This function add a new column `.outside_xy` in the returned dataset
#'
#' @param data a data.frame with the default column names: "database_id", "scientificName", "decimalLongitude", "decimalLatitude"
#' 
#' @importFrom dplyr mutate case_when
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data %>%
#'   bdc_flag_outside_xy()
#' }
bdc_flag_outside_xy  <- function(data) {

  data <-
    data %>%
    dplyr::mutate(
      .outside_xy = dplyr::case_when(
        decimalLatitude < -90 | decimalLatitude > 90 ~ TRUE,
        decimalLongitude < -180 | decimalLongitude > 180 ~ TRUE,
        TRUE ~ FALSE
      )
    )

  return(data)

}
