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

  data_filtered <-
    data %>%
    dplyr::select(.data[[lon]], .data[[lat]]) %>%
    dplyr::rename(lon = .data[[lon]], lat = .data[[lat]]) %>%
    dplyr::mutate_all(as.numeric)

  data_flag <-
    data_filtered %>%
    dplyr::mutate(
      .invalid_xy = dplyr::case_when(
        lat < -90 | lat > 90 ~ FALSE,
        lon < -180 | lon > 180 ~ FALSE,
        TRUE ~ TRUE
      )
    ) %>%
    dplyr::select(.invalid_xy)

  df <- dplyr::bind_cols(data,  data_flag)
  
  message(
    paste(
      "\nbdc_flag_invalid_xy:\nFlagged",
      sum(df$.invalid_xy == FALSE),
      "records.\nOne column was added to the database.\n"))

  return(df)

}
