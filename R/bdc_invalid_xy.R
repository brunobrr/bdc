#' Identify records with invalid geographic coordinates
#'
#' Flags records with out-of-range coordinates (-90 to 90 for latitude -180 to 180 for longitude).
#'
#' @param data data.frame. Containing geographical coordinates. Coordinates must be expressed in decimal degree and in WGS84. 
#' @param lat character string. The column name with latitude data. Default =
#' "decimalLatitude".
#' @param lon character string. The column name with longitude data. Default =
#' "decimalLongitude".
#' 
#' @return A data.frame contain the column '.invalid_xy'. Records that have
#' failed in the test are flagged as "FALSE".
#' 
#' @importFrom dplyr select rename mutate_all mutate case_when bind_cols
#' 
#' @export
#'   
#' @examples
#' \dontrun{
#' decimalLatitude <- c(-185.111, -43.34, "", -21.8069444)
#' decimalLongitude <- c(-45.4, -39.6, -20.5243, -440.9055555)
#' x <- data.frame(decimalLatitude, decimalLongitude)
#' bdc_invalid_xy(data = x, lat = "decimalLatitude", lon = "decimalLongitude")
#' }
bdc_invalid_xy  <- 
  function(data, lat = "decimalLatitude", lon = "decimalLongitude") {

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
      "\nbdc_invalid_xy:\nFlagged",
      sum(df$.invalid_xy == FALSE),
      "records.\nOne column was added to the database.\n"))

  return(df)

}
