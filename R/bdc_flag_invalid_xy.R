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
bdc_flag_invalid_xy  <- function(data, long, lat) {

  data <-
    data %>%
    dplyr::mutate(intid = 1:dplyr::n())

  data_filtered <-
    data %>%
    dplyr::select(intid, .data[[long]], .data[[lat]]) %>%
    dplyr::rename(long = .data[[long]], lat = .data[[lat]]) %>%
    dplyr::mutate_all(as.numeric)

  data_flag <-
    data_filtered %>%
    dplyr::mutate(
      .invalid_xy = dplyr::case_when(
        lat < -90 | lat > 90 ~ TRUE,
        is.na(lat) | is.na(lat) ~ TRUE,
        long < -180 | long > 180 ~ TRUE,
        is.na(long) | is.na(long)~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    select(intid, .invalid_xy)

  return(dplyr::full_join(data, data_flag) %>% dplyr::select(-intid))

}
