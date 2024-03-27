#' Flag low-precise geographic coordinates
#'
#' This function flags records with a coordinate precision below a
#' specified number of decimal places. Coordinates with one, two, or three
#' decimal places present a precision of~11.1 km, ~1.1 km, and ~111 m at the
#' equator, respectively.
#'
#' @family space
#' @param data data.frame. A data.frame containing geographic coordinates.
#' @param lon character string. The column with longitude in decimal degrees and
#' WGS84. Default = "decimalLongitude".
#' @param lat character string. The column with latitude in decimal degrees and
#' WGS84. Default = "decimalLatitude".
#' @param ndec numeric. The minimum number of decimal places that the
#' coordinates should
#' have to be considered valid. Default = 2.
#'
#' @return A data.frame with logical values indicating whether values are equal
#' or higher than the specified minimum decimal number (ndec). Coordinates
#' flagged as FALSE in .rou column are considered imprecise.
#'
#' @importFrom dplyr select bind_cols rename
#' @importFrom stringr str_split_fixed str_length
#'
#' @export
#'
#' @examples
#' x <- data.frame(
#'   lat = c(-21.34, 23.567, 16.798, -10.468),
#'   lon = c(-55.38, -13.897, 30.8, 90.675)
#' )
#'
#' bdc_coordinates_precision(
#'   data = x,
#'   lat = "lat",
#'   lon = "lon",
#'   ndec = 3
#' )
#' 
bdc_coordinates_precision <-
  function(data,
           lat = "decimalLatitude",
           lon = "decimalLongitude",
           ndec = c(0, 1, 2)) {
    . <- .ndec_all <- NULL

    df <-
      data %>%
      dplyr::select({{ lon }}, {{ lat }}) %>%
      as.data.frame()

    ndec_lat <- (df[, lat] %>%
      as.character() %>%
      stringr::str_split_fixed(., pattern = "[.]", n = 2))[, 2] %>%
      stringr::str_length()

    ndec_lon <- (df[, lon] %>%
      as.character() %>%
      stringr::str_split_fixed(., pattern = "[.]", n = 2))[, 2] %>%
      stringr::str_length()

    rm(df)

    ndec_list <- as.list(ndec)
    names(ndec_list) <- paste0(".", "ndec", ndec)

    for (i in 1:length(ndec)) {
      ndec_list[[i]] <- (ndec_lat >= ndec[i] & ndec_lon >= ndec[i])
    }
    ndec_list <- dplyr::bind_cols(ndec_list)
    ndec_list$.ndec_all <- apply(ndec_list, 1, all) # all flagged as low decimal precision

    ndec_list <-
      ndec_list %>%
      dplyr::select(.ndec_all) %>%
      dplyr::rename(.rou = .ndec_all)

    message("bdc_coordinates_precision:\nFlagged ", sum(!ndec_list[".rou"]), " records\nOne column was added to the database.\n")

    res <- dplyr::bind_cols(data, ndec_list)

    return(res)
  }
