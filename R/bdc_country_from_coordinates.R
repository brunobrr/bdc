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
#' @return A data.frame containing country names for records missing such
#' information.
#'
#' @importFrom CoordinateCleaner cc_val cc_sea
#' @importFrom dplyr mutate filter select
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

    suppressWarnings({
    check_require_cran("rnaturalearth")
    check_require_github("ropensci/rnaturalearthdata")
    })
    
    # create an id
    data$id <- 1:nrow(data)
    
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
    
    if (!has_country){
      data$country <- NA
    }
      
    # converts coordinates columns to numeric
    data <-
      data %>%
      dplyr::mutate(
        decimalLatitude = as.numeric(.data[[lat]]),
        decimalLongitude = as.numeric(.data[[lon]])
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
      suppressWarnings({
        suppressMessages({
          ext_country <-
            data_sp %>%
            dplyr::select(id, geometry) %>%
            sf::st_intersection(., worldmap)
        })
      })
      
      ext_country$geometry <- NULL
      w <- which(data$id %in% ext_country$id)

      data[w, "country"] <- ext_country$name_long
    }

    data <- data %>% dplyr::select(-id)

    if (has_country) {
      message(
        paste(
          "\nbdc_country_from_coordinates:\nCountry names were added to",
          length(w),
          "records.\n"
        ))
    } else {
      message(
      paste(
        "\nbdc_country_from_coordinates:\nCountry names were added to",
        length(w),
        "records in a new collumn named 'country'.\n"
      ))
    }
    
    return(data)
  }
