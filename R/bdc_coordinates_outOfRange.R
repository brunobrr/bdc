#' Identify records with out-of-range geographic coordinates
#'
#' This function identifies records with out-of-range coordinates (not between
#' -90 and 90 for latitude; between -180 and 180 for longitude).
#'
#' @family prefilter
#' @param data data.frame. Containing geographical coordinates. Coordinates must
#' be expressed in decimal degrees and WGS84.
#' @param lat character string. The column name with latitude in decimal degree
#' and in WGS84. Default = "decimalLatitude".
#' @param lon character string. The column with longitude in decimal degree and
#' in WGS84. Default = "decimalLongitude".
#'
#' @return A data.frame containing the column ".coordinates_outOfRange".
#' Compliant (TRUE) if 'lat' and 'lon' are not out-of-range; otherwise
#' "FALSE".
#'
#' @importFrom dplyr select rename mutate case_when bind_cols all_of across everything
#'
#' @export
#'
#' @examples
#' x <- data.frame(
#'   decimalLatitude = c(-185.111, -43.34, "", -21.8069444),
#'   decimalLongitude = c(-45.4, -39.6, -20.5243, -440.9055555)
#' )
#'
#' bdc_coordinates_outOfRange(
#'   data = x,
#'   lat = "decimalLatitude",
#'   lon = "decimalLongitude"
#' )
#' 
bdc_coordinates_outOfRange <-
  function(data,
           lat = "decimalLatitude",
           lon = "decimalLongitude") {
    .data <- .coordinates_outOfRange <- NULL

    data_filtered <-
      data %>%
      dplyr::select(dplyr::all_of(c(lon, lat))) %>%
      dplyr::rename(lon = dplyr::all_of(lon), lat = dplyr::all_of(lat)) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ as.numeric(.x)))

    data_flag <-
      data_filtered %>%
      dplyr::mutate(
        .coordinates_outOfRange = dplyr::case_when(
          lat < -90 | lat > 90 ~ FALSE,
          lon < -180 | lon > 180 ~ FALSE,
          TRUE ~ TRUE
        )
      ) %>%
      dplyr::select(.coordinates_outOfRange)

    df <- dplyr::bind_cols(data, data_flag)

    message(
      paste(
        "\nbdc_coordinates_outOfRange:\nFlagged",
        sum(df$.coordinates_outOfRange == FALSE),
        "records.\nOne column was added to the database.\n"
      )
    )

    return(df)
  }
