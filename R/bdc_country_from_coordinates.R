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
#' of each record. Default = "country". If `data` has no column with this
#' name, a new column with this name is created.
#'
#' @details This function assigns a country name for records missing such
#' information. Country names are extracted from valid geographic coordinates
#' using a high-quality map of the world (rnaturalearth package). No
#' country name is added to records whose coordinates are in the sea.
#'
#' @return A tibble with the columns of `data`, where missing values in the
#' column named by `country` are filled with country names derived from
#' coordinates. If that column did not exist, it is added; no other column is
#' added.
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
    decimalLatitude <- decimalLongitude <- NULL
    
    check_require_cran("rnaturalearthdata")

    # spherical geometry (s2) is turned off below; restore it on exit
    old_s2 <- sf::sf_use_s2()
    on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)
    
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
      data[[country]] <- NA
    }
    
    # Work on a helper table so the user's columns are left untouched: a
    # temporary id plus numeric coordinates with fixed internal names.
    coords <-
      data.frame(
        id_temp = seq_len(nrow(data)),
        decimalLatitude = suppressWarnings(as.numeric(data[[lat]])),
        decimalLongitude = suppressWarnings(as.numeric(data[[lon]]))
      )
    
    worldmap <-
      rnaturalearth::ne_countries(scale = "large", returnclass = "sf") %>%
      bdc_reword_countries()

    no_country <- is.na(data[[country]]) | data[[country]] == ""
    
    data_no_country <-
      coords %>%
      dplyr::filter(
        no_country,
        !is.na(decimalLatitude),
        !is.na(decimalLongitude)
      )
    
    if (nrow(data_no_country) == 0) {
      message("All records already had country information. Nothing was done!")
    } else{
      # converts coordinates columns to spatial points
      suppressWarnings({
        data_no_country <-
          CoordinateCleaner::cc_val(
            x = data_no_country,
            lon = "decimalLongitude",
            lat = "decimalLatitude",
            verbose = FALSE
          ) %>%
          CoordinateCleaner::cc_sea(
            x = .,
            lon = "decimalLongitude",
            lat = "decimalLatitude",
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
      
      data[[country]][res$id_temp] <- res$name_long
      
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
            paste0("records in a new collumn named '", country, "'.\n")
          )
        )
      }
      
    }
    data[[country]][data[[country]] %in% c("", "NA")] <- NA
  
    return(dplyr::as_tibble(data))
    
  }
