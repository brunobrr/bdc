#' Get country names from coordinates
#'
#' Country names derived from valid geographic coordinates are added to records
#' missing country names.
#'
#' @param data data.frame. Containing geographical coordinates and country
#' names. Coordinates must be expressed in decimal degree and in WGS84.
#' @param lat character string. The column name with latitude. Coordinates must
#' be expressed in decimal degree and in WGS84. Default = "decimalLatitude".
#' @param lon character string. The column with longitude. Coordinates must be
#' expressed in decimal degree and in WGS84. Default = "decimalLongitude".
#' @param  country character string. The column name with the country assignment
#' of each record. Default = "country".
#' 
#' @details This function assigns country name for records missing such
#' information. Country names are extracted from valid geographic coordinates
#' by using a high-quality map of the world (rnaturalearth package). No
#' country name is added to records whose coordinates are in the sea.
#' 
#' @return A data.frame containing country names for records missing such 
#' information.
#' 
#' @importFrom CoordinateCleaner cc_val cc_sea
#' @importFrom dplyr filter left_join contains pull rename
#' @importFrom readr write_csv
#' @importFrom rnaturalearth ne_countries
#' @importFrom sf st_as_sf st_set_crs st_crs st_intersection 
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' decimalLatitude <- c(-63.43333, -67.91667, -41.90000, -46.69778)
#' decimalLongitude <- c(-17.90000, -14.43333, -13.25000, -13.82444)
#' country <- c("", "NA", NA, "Brazil")
#' 
#' x <- data.frame(decimalLatitude, decimalLongitude, country)
#' 
#' bdc_country_from_coordinates(
#'   data = x,
#'   lat = "decimalLatitude",
#'   lon = "decimalLongitude",
#'   country = "country")
#' }
#' 
bdc_country_from_coordinates <-
  function(data,
           lat = "decimalLatitude",
           lon = "decimalLongitude",
           country = "country") {
    # create an id
    data$id <- 1:nrow(data)

    minimum_colnames <- c(lat, lon, country)

    if (length(minimum_colnames) < 3) {
      stop("Fill all function arguments: data, id, lon, lat, and
         country")
    }

    if (!all(minimum_colnames %in% colnames(data))) {
      stop(
        "These columns names were not found in your database: ",
        paste(minimum_colnames[!minimum_colnames %in% colnames(data)],
          collapse = ", "
        ),
        call. = FALSE
      )
    }

    # converts coordinates columns to numeric
    data <-
      data %>%
      dplyr::mutate(
        decimalLatitude = as.numeric(decimalLatitude),
        decimalLongitude = as.numeric(decimalLongitude)
      )


    worldmap <- rnaturalearth::ne_countries(scale = "large")

    data_no_country <-
      data %>%
      dplyr::filter(is.na(country) | country == "")

    if (nrow(data_no_country) > 0) {
      # converts coordinates columns to spatial points
      suppressWarnings({
        data_sp <-
          CoordinateCleaner::cc_val(
            x = data_no_country,
            lon = lon,
            lat = lat,
            verbose = FALSE
          ) %>%
          CoordinateCleaner::cc_sea(
            x = .,
            lon = lon,
            lat = lat,
            verbose = FALSE,
            speedup = TRUE,
            ref = worldmap
          ) %>%
          sf::st_as_sf(.,
            coords = c("decimalLongitude", "decimalLatitude"),
            remove = FALSE
          ) %>%
          sf::st_set_crs(., sf::st_crs(worldmap))
      })

      worldmap <- sf::st_as_sf(worldmap) %>% dplyr::select(name_long)

      # Extract country names from coordinates
      suppressMessages({
        ext_country <-
          data_sp %>%
          dplyr::select(id, geometry) %>%
          sf::st_intersection(., worldmap)
      })

      ext_country$geometry <- NULL
      w <- which(data$id %in% ext_country$id)

      data[w, "country"] <- ext_country$name_long
    }

    data <- data %>% dplyr::select(-id)
    
    message(
      paste(
        "\nbdc_country_from_coordinates:\nCountry names were added to",
        length(w),
        "records.\n"
      )
    )

    return(data)
  }
