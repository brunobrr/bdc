#' Title Identify records in a country or at a determined distance from the country (e.g., in the ocean)
#'
#' @param data data.frame. Containing longitude and latitude
#' @param country_name character string. Name of the country to considered. 
#' @param lon character string. The column with the longitude coordinates. Default = “decimallatitude”.
#' @param lat character string. The column with the latitude coordinates. Default = “decimallatitude”.
#' @param dist numeric. The distance in degrees used to created a buffer around the country. 
#' @details Records within a buffer around a country but not in other countries (e.g., records in the ocean) are flagged as TRUE (test passed). This avoids to flag as FALSE records close to country limits. For example, records of coast or marshland species.
#'
#' @return
#' @export
#'
#' @examples
bdc_xy_in_country <- function(data,
                              country_name,
                              lon = "decimalLongitude",
                              lat = "decimalLatitude",
                              dist = 0.5) {
  data <-
    data %>%
    dplyr::select(.data[[lon]], .data[[lat]]) %>%
    dplyr::mutate(id = 1:nrow(data))


  # get country limits
  country_shp <-
    rnaturalearth::ne_countries(
      country = country_name,
      scale = "large",
      returnclass = "sf"
    )
  # Spatial points
  data <-
    CoordinateCleaner::cc_val(
      x = data,
      lon = "decimalLongitude",
      lat = "decimalLatitude"
    ) %>%
    sf::st_as_sf(.,
      coords = c("decimalLongitude", "decimalLatitude"),
      remove = F
    ) %>%
    sf::st_set_crs(., st_crs(country_shp))


  # buffer
  buf <- sf::st_buffer(country_shp, dist = dist)


  # Extract points within the buffer
  data <-
    data %>%
    dplyr::mutate(points_in_buf = st_intersects(data, buf, sparse = F))

  # Filter points within the buffer
  data_fil <-
    data %>%
    dplyr::filter(points_in_buf == TRUE)

  # Points in other countries
  all_countries <-
    rnaturalearth::ne_countries(returnclass = "sf") %>%
    dplyr::select(name_long) %>%
    st_crop(., st_bbox(data_fil)) # Crop according to points bbox

  # Extract country names from points
  ext_country <- st_intersection(data, all_countries)
  data$geometry <- NULL
  ext_country$geometry <- NULL

  names_to_join <-
    ext_country %>%
    dplyr::select(id, name_long)

  data_join <-
    dplyr::full_join(data, names_to_join, by = "id") %>%
    mutate(
      .xy_in_country =
        case_when(
          (points_in_buf == TRUE & is.na(name_long)) ~ TRUE,
          (points_in_buf == FALSE) ~ FALSE,
          (points_in_buf == TRUE & name_long != country_name) ~ FALSE,
          (points_in_buf == TRUE & name_long == country_name) ~ TRUE
        )
    )

  message(paste("Flagged", sum(data_join$.xy_in_country == FALSE), "out", nrow(data_join), "records"))

  return(data_join %>% pull(.xy_in_country))

  # test <-
  #   st_as_sf(
  #     data_join,
  #     coords = c("decimalLongitude", "decimalLatitude"),
  #     crs = st_crs(country_shp)
  #   )
  #
  # plot(country_shp$geometry)
  # plot(test[".points_in_country"], pch = 19, cex = 0.1, add=T)
  # plot(country_shp$geometry, add=T)
  # plot(buf$geometry, add=T, border = "red")
}
