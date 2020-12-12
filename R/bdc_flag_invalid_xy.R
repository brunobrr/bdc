#' Flag invalid coordinates
#'
#' @description
#' This function add a new column `.invalid_xy` in the returned dataset
#'
#' @param data a data.frame with the default column names: "database_id", 
#' "scientificName", "decimalLongitude", "decimalLatitude"
#'
#' @importFrom dplyr mutate n select rename mutate_all case_when full_join
#'
#' @export
#'
bdc_flag_invalid_xy  <- function(data, lon, lat) {

  data <-
    data %>%
    dplyr::mutate(intid = 1:dplyr::n())

  data_filtered <-
    data %>%
    dplyr::select(intid, .data[[lon]], .data[[lat]]) %>%
    dplyr::rename(lon = .data[[lon]], lat = .data[[lat]]) %>%
    dplyr::mutate_all(as.numeric)

  data_flag <-
    data_filtered %>%
    dplyr::mutate(
      .invalid_xy = dplyr::case_when(
        lat < -90 | lat > 90 ~ FALSE,
        is.na(lat) | is.na(lat) ~ FALSE,
        lon < -180 | lon > 180 ~ FALSE,
        is.na(lon) | is.na(lon)~ FALSE,
        TRUE ~ TRUE
      )
    ) %>%
    dplyr::select(intid, .invalid_xy)

  return(data_flag %>% dplyr::pull(.invalid_xy))

}
