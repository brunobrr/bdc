#' Identify records outside a country or at a determined distance from the country
#' (e.g., in the ocean)
#'
#' This function identify/flag geographic coordinates using a buffer for the
#' country chosen by the user. See @details.
#'
#' @param data data.frame. Containing longitude and latitude. Coordinates must be expressed in decimal degree and in WGS84. 
#' @param country_name character string. Name of the country to considered. 
#' @param lon character string. The column name with the longitude coordinates. Default = “decimallatitude”.
#' @param lat character string. The column name with the latitude coordinates. Default = “decimallatitude”.
#' @param dist numeric. The distance in decimal degrees used to created a buffer around the country. 
#' 
#' @details Records within a buffer around a country but not in other countries
#' (e.g., records in the ocean) are flagged as TRUE (test passed). This avoids
#' to flag as FALSE records close to country limits. For example, records of
#' coast or marshland species.
#'
#' @return a data.frame with flagged records in column `.xy_out_country`
#' 
#' @importFrom CoordinateCleaner cc_val
#' @importFrom dplyr select mutate filter full_join bind_cols
#' @importFrom rnaturalearth ne_countries
#' @importFrom sf st_as_sf st_set_crs st_bbox st_buffer st_crop st_intersects
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' decimalLatitude <- c(19.9358, -13.016667, NA, "")
#' decimalLongitude <- c(-40.6003, -39.6, -20.5243, NA)
#' x <- data.frame(decimalLatitude, decimalLongitude)
#' bdc_country_xy_mismatch(
#'   data = x,
#'   country_name = "Brazil",
#'   lon = "decimalLongitude",
#'   lat = "decimalLatitude",
#'   dist = 0.5 # in decimal degrees
#' )
#' }
bdc_country_xy_mismatch <-
  function(data,
           country_name,
           lon = "decimalLongitude",
           lat = "decimalLatitude",
           dist = 0.5) {
    
  df <-
    data %>%
    dplyr::select(.data[[lon]], .data[[lat]]) %>%
    dplyr::mutate(id = 1:nrow(data))

  # get country limits
  country_shp <-
    rnaturalearth::ne_countries(
      country = country_name,
      scale = "large",
      returnclass = "sf")
  
  # Spatial points
  data_sp <-
    CoordinateCleaner::cc_val(
      x = df,
      lon = "decimalLongitude",
      lat = "decimalLatitude",
      verbose = F
    ) %>%
    sf::st_as_sf(.,
      coords = c("decimalLongitude", "decimalLatitude"),
      remove = F
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
      dplyr::mutate(points_in_buf = sf::st_intersects(data_sp, buf, sparse = F))
  })

  # Filter points within the buffer
  data_fil <-
    data_sp %>%
    dplyr::filter(points_in_buf == TRUE)

  # Points in other countries
  suppressMessages({
    suppressWarnings({
      all_countries <-
        rnaturalearth::ne_countries(returnclass = "sf") %>%
        dplyr::select(name_long) %>%
        sf::st_crop(., sf::st_bbox(data_fil)) # Crop according to points bbox
    })
  })

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
      .xy_out_country =
        dplyr::case_when(
          (points_in_buf == TRUE & is.na(name_long)) ~ TRUE,
          (points_in_buf == FALSE) ~ FALSE,
          (points_in_buf == TRUE &
            name_long != country_name) ~ FALSE,
          (points_in_buf == TRUE & name_long == country_name) ~ TRUE
        )
    ) %>%
    dplyr::select(id, .xy_out_country)

  data_join <-
    dplyr::full_join(df, data_to_join, by = "id") %>%
    dplyr::mutate(
      .xy_out_country =
        ifelse(is.na(.xy_out_country), FALSE, .xy_out_country)
    ) %>%
    dplyr::select(.xy_out_country)

  df <- dplyr::bind_cols(data, data_join)

  message(
    paste(
      "\nbdc_country_xy_mismatch:\nFlagged",
      sum(df$.xy_out_country == FALSE),
      "records.\nOne column was added to the database.\n"
    )
  )

  return(df)
}
