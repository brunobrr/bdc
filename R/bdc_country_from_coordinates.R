#' Get country names from coordinates
#'
#' Country names derived from valid geographic coordinates are added to records
#' missing country names.
#'
#' @param data data.frame. Containing geographical coordinates and country
#' names.
#' @param lat character string. The column name with latitude in decimal
#' degrees
#' and WGS84. Default = "decimalLatitude".
#' @param lon character string. The column with longitude in decimal degrees and
#' WGS84. Default = "decimalLongitude".
#' @param  country character string. The column name with the country assignment
#' of each record. Default = "country". If no column name is provided a new
#' column "country" is created.
#'
#' @details This function assigns a country name for records missing such
#' information. Country names are extracted from valid geographic coordinates
#' using a high-quality map of the world (rnaturalearth package). No
#' country name is added to records whose coordinates are in the sea.
#'
#' @return A tibble containing country names for records missing such
#' information.
#'
#' @importFrom CoordinateCleaner cc_val cc_sea
#' @importFrom dplyr mutate filter select left_join as_tibble 
#' @importFrom rnaturalearth ne_countries
#' @importFrom sf st_as_sf st_set_crs st_crs st_intersection as_Spatial sf_use_s2
#'
#' @export
#'
#' @examples
#' \dontrun{
#' x <- data.frame(
#'   decimalLatitude = c(-22.9834, -39.857030, -17.06811, -46.69778),
#'   decimalLongitude = c(-69.095, -68.443588, 37.438108, -13.82444),
#'   country = c("", NA, NA, "Brazil"))
#'
#' bdc_country_from_coordinates(
#'   data = x,
#'   lat = "decimalLatitude",
#'   lon = "decimalLongitude",
#'   country = "country"
#' )
#' }
bdc_country_from_coordinates <-
  function(data,
           lat = "decimalLatitude",
           lon = "decimalLongitude",
           country = "country") {
    .data <- . <- name_long <- id_temp <- geometry <- NULL
    
    suppressWarnings({
      check_require_cran("rnaturalearth")
      check_require_github("ropensci/rnaturalearthdata")
    })
    
    # create an id_temp
    data$id_temp <- 1:nrow(data)
    
    minimum_colnames <- c(lat, lon)
    
    if (!all(minimum_colnames %in% colnames(data))) {
      stop(
        "These columns names were not found in your database: ",
        paste(minimum_colnames[!minimum_colnames %in% colnames(data)],
              collapse = ", "),
        call. = FALSE
      )
    }
    
    # check if data has a country column
    has_country <- any(colnames(data) == country)
    
    if (!has_country) {
      data$country <- NA
    }
    
    # converts coordinates columns to numeric
    data <-
      data %>%
      dplyr::mutate(decimalLatitude = as.numeric(.data[[lat]]),
                    decimalLongitude = as.numeric(.data[[lon]]))
    
    worldmap <-
      rnaturalearth::ne_countries(scale = "large", returnclass = "sf") %>%
      bdc_reword_countries()

    data_no_country <-
      data %>%
      dplyr::filter(is.na(country) | country == "")
    
    data_no_country <- data_no_country %>% 
      dplyr::filter(!is.na(data_no_country[[lat]]), !is.na(data_no_country[[lon]]))
    
    if (nrow(data_no_country) == 0) {
      data <- data %>% dplyr::select(-id_temp)
      message("All records already had country information. Nothing was done!")
    } else{
      # converts coordinates columns to spatial points
      suppressWarnings({
        data_no_country <-
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
            ref = sf::as_Spatial(worldmap)
          ) %>%
          sf::st_as_sf(
            .,
            coords = c("decimalLongitude", "decimalLatitude"),
            remove = FALSE
          ) %>%
          sf::st_set_crs(., sf::st_crs(worldmap))
      })
      
      # Extract country names from coordinates
      suppressWarnings({
        suppressMessages({
          sf::sf_use_s2(FALSE)
          ext_country <-
            data_no_country %>%
            dplyr::select(id_temp, geometry) %>%
            sf::st_intersection(., worldmap)
        })
      })
      
      ext_country$geometry <- NULL
      
      res <-
        dplyr::left_join(data_no_country, ext_country, by = "id_temp")
      
      id_replace <- res$id_temp
      data[id_replace, "country"] <- res$name_long
      data <- data %>% dplyr::select(-id_temp)
      
      if (has_country) {
        message(
          paste(
            "\nbdc_country_from_coordinates:\nCountry names were added to",
            nrow(data_no_country),
            "records.\n"
          )
        )
      } else {
        message(
          paste(
            "\nbdc_country_from_coordinates:\nCountry names were added to",
            nrow(data_no_country),
            "records in a new collumn named 'country'.\n"
          )
        )
      }
      
    }
    data[,country][data[,country]%in%c("", "NA")] <- NA
  
    return(dplyr::as_tibble(data))
    
  }
