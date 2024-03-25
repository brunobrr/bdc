#' Identify records with empty geographic coordinates
#'
#' This function flags records missing latitude or longitude coordinates.
#'
#' @family prefilter
#' @param data data.frame. Containing geographical coordinates.
#' @param lat character string. The column name with latitude in decimal degrees
#' and WGS84. Default = "decimalLatitude".
#' @param lon character string. The column with longitude in decimal degrees and
#' WGS84. Default = "decimalLongitude".
#'
#' @details This test identifies records missing geographic coordinates (i.e.,
#' empty or not applicable [NA] longitude or latitude)
#'
#' @return A data.frame containing the column ".coordinates_empty". Compliant
#' (TRUE) if 'lat' and 'lon' are not empty; otherwise "FALSE".
#'
#' @importFrom dplyr across mutate case_when select bind_cols all_of if_any
#'
#' @export
#'
#' @examples
#' x <- data.frame(
#'   decimalLatitude = c(19.9358, -13.016667, NA, ""),
#'   decimalLongitude = c(-40.6003, -39.6, -20.5243, NA)
#' )
#'
#' bdc_coordinates_empty(
#'   data = x,
#'   lat = "decimalLatitude",
#'   lon = "decimalLongitude"
#' )
#'
bdc_coordinates_empty <-
  function(data,
           lat = "decimalLatitude",
           lon = "decimalLongitude") {
    .coordinates_empt <- .data <- .coordinates_empty <- .coordinates_outOfRange <- isna_ <- NULL

    if (!is.data.frame(data)) {
      stop(deparse(substitute(data)), " is not a data.frame", call. = FALSE)
    }

    check_col(data, c(lat, lon))

    df <-
      data %>%
      dplyr::select(dplyr::all_of(c(lat, lon))) %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(c(lat, lon)), ~ as.numeric(.x))) %>%
      dplyr::mutate(
        isna_ = dplyr::if_any(dplyr::all_of(c(lat, lon)), ~ is.na(.x)),
        .coordinates_empty = ifelse(isna_, FALSE, TRUE)
      ) %>%
      dplyr::select(.coordinates_empty)

    df <- dplyr::bind_cols(data, df)

    message(
      paste(
        "\nbdc_coordinates_empty:\nFlagged",
        sum(df$.coordinates_empty == FALSE),
        "records.\nOne column was added to the database.\n"
      )
    )

    return(df)
  }
