#' Identify records within a reference country
#'
#' This function flags geographic coordinates within a reference country. A
#' spatial buffer can be added to the reference country to ensure that
#' records in mangroves, marshes, estuaries, and records with low
#' coordinate precision are not flagged as invalid.
#'
#' @family prefilter
#' @param data data.frame. Containing longitude and latitude. Coordinates must
#' be expressed in decimal degrees and WGS84.
#' @param country_name character string. Name of the country to considered.
#' @param lat character string. The column name with the latitude coordinates.
#' Default = “decimallatitude”.
#' @param lon character string. The column name with the longitude coordinates.
#' Default = “decimallongitude”.
#' @param dist numeric. The distance in decimal degrees used to created a buffer
#' around the country. Default = 0.1 (~11 km at the equator).
#'
#' @details The distance informed in the argument 'dist' is used to create a
#' buffer around the reference country. Records within the reference country
#' or at a specified distance from the coastline of the reference country
#' (i.e., records within the buffer) are flagged as valid (TRUE). Note that
#' records within the buffer but in other countries are flagged as invalid
#' (FALSE).
#'
#' @return A data.frame containing the column
#' '.coordinates_country_inconsistent'. Compliant (TRUE) if coordinates fall
#' within the boundaries plus a specified distance (if 'dist' is supplied) of
#' 'country_name'; otherwise "FALSE".
#'
#' @importFrom dplyr select mutate filter full_join case_when left_join bind_cols
#' @importFrom rnaturalearth ne_countries
#' @importFrom sf sf_use_s2 st_as_sf st_set_crs st_crs st_buffer st_intersects
#' st_intersection
#'
#' @export
#'
#' @examples
#' \donttest{
#' x <- data.frame(
#'   decimalLongitude = c(-40.6003, -39.6, -20.5243, NA),
#'   decimalLatitude = c(19.9358, -13.016667, NA, "")
#'   )
#'   
#' bdc_coordinates_country_inconsistent(
#'   data = x,
#'   country_name = "Brazil",
#'   lon = "decimalLongitude",
#'   lat = "decimalLatitude",
#'   dist = 0.1 # in decimal degrees
#' )
#'
#' bdc_coordinates_country_inconsistent(
#'   data = x,
#'   country_name = c("Argentina", "Brazil"),
#'   lon = "decimalLongitude",
#'   lat = "decimalLatitude",
#'   dist = 0.2 # in decimal degrees
#' )
#' }
bdc_coordinates_country_inconsistent <-
  function(data,
           country_name,
           lat = "decimalLatitude",
           lon = "decimalLongitude",
           dist = 0.1) {
    .data <- .summary <- .coordinates_empty <- .coordinates_outOfRange <- . <- NULL
    points_in_buf <- name_long <- id <- .coordinates_country_inconsistent <- NULL

    check_require_cran("rnaturalearth")
    sf::sf_use_s2(FALSE)

    df <-
      data %>%
      dplyr::select(.data[[lon]], .data[[lat]]) %>%
      dplyr::mutate(id = 1:nrow(data))

    # identifying empty or out-of-range coordinates
    suppressMessages({
      data_raw <-
        bdc_coordinates_empty(
          data = df,
          lat = {{ lat }},
          lon = {{ lon }}
        )

      data_raw <-
        bdc_coordinates_outOfRange(
          data = data_raw,
          lat = {{ lat }},
          lon = {{ lon }}
        )

      data_raw <- bdc_summary_col(data_raw)
    })

    df <-
      data_raw %>%
      dplyr::filter(.summary == TRUE)

    df <-
      df %>%
      dplyr::select(-c(.coordinates_empty, .coordinates_outOfRange, .summary))


    # get country limits
    country_shp <-
      rnaturalearth::ne_countries(
        country = country_name,
        scale = "large",
        returnclass = "sf"
      )


    # Spatial points
    data_sp <-
      sf::st_as_sf(
        df,
        coords = c("decimalLongitude", "decimalLatitude"),
        remove = FALSE
      ) %>%
      sf::st_set_crs(., sf::st_crs(country_shp))


    # buffer
    suppressWarnings({
      buf <- sf::st_buffer(country_shp, dist = dist)
    })

    # Extract points within the buffer
    suppressMessages({
      data_sp <-
        data_sp %>%
        dplyr::mutate(points_in_buf = sf::st_intersects(data_sp, buf, sparse = FALSE))
    })


    # Remove additional columns within 'points_in_buf' object
    if (length(country_name) > 1) {
      data_sp$points_in_buf <- apply(data_sp$points_in_buf, 1, any)
    }


    # Points in other countries
    all_countries <-
      rnaturalearth::ne_countries(returnclass = "sf", scale = "large") %>%
      dplyr::select(name_long)

    # Extract country names from points
    suppressWarnings({
      ext_country <- sf::st_intersection(data_sp, all_countries)
    })
    data_sp$geometry <- NULL
    ext_country$geometry <- NULL

    names_to_join <-
      ext_country %>%
      dplyr::select(id, name_long)

    data_to_join <-
      dplyr::full_join(data_sp, names_to_join, by = "id") %>%
      dplyr::mutate(
        .coordinates_country_inconsistent =
          dplyr::case_when(
            (points_in_buf == TRUE & is.na(name_long)) ~ TRUE,
            (points_in_buf == FALSE) ~ FALSE,
            (points_in_buf == TRUE &
              name_long != country_name) ~ FALSE,
            (points_in_buf == TRUE & name_long == country_name) ~ TRUE
          )
      ) %>%
      dplyr::select(id, .coordinates_country_inconsistent)

    data_raw <-
      dplyr::left_join(data_raw, data_to_join, by = "id")

    data_raw$.coordinates_country_inconsistent <-
      ifelse(
        is.na(data_raw$.coordinates_country_inconsistent),
        TRUE,
        data_raw$.coordinates_country_inconsistent
      )

    data_raw$.coordinates_country_inconsistent <-
      ifelse(
        data_raw$.summary == FALSE,
        TRUE,
        data_raw$.coordinates_country_inconsistent
      )

    data_raw <- data_raw %>% dplyr::select(.coordinates_country_inconsistent)

    df <- dplyr::bind_cols(data, data_raw)

    message(
      paste(
        "\nbdc_coordinates_country_inconsistent:\nFlagged",
        sum(df$.coordinates_country_inconsistent == FALSE),
        "records.\nOne column was added to the database.\n"
      )
    )

    return(df)
  }
