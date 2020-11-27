# TODO: run prefixer to catch namespaces and importFrom

#' Flag missing coordinates
#'
#' @description
#' This function add a new column `.no_xy` in the returned dataset
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
bdc_flag_no_xy <- function(data) {

  data <-
    data %>%
    dplyr::mutate(
      .no_xy = dplyr::case_when(
        is.na(decimalLatitude) ~ TRUE,
        is.na(decimalLongitude) ~ TRUE,
        # flag empty coordinates
        nzchar(decimalLatitude) == FALSE ~ TRUE,
        nzchar(decimalLongitude) == FALSE ~ TRUE,
        # opposite cases are flagged as FALSE
        TRUE ~ FALSE
      )
    )

  return(data)

}
