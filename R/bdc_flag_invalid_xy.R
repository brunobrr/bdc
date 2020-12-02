#' Flag invalid coordinates
#'
#' @description
#' This function add a new column `.invalid_xy` in the returned dataset
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
#'   bdc_flag_invalid_xy()
#' }
bdc_flag_invalid_xy <- function(data, long, lat) {

  data <-
    data %>%
    dplyr::mutate(
      .invalid_xy = dplyr::case_when(
        is.na(lat) ~ TRUE,
        is.na(long) ~ TRUE,
        # flag empty coordinates
        nzchar(lat) == FALSE ~ TRUE,
        nzchar(long) == FALSE ~ TRUE,
        # opposite cases are flagged as FALSE
        TRUE ~ FALSE
      )
    )

  return(data)

}
