#' Identify transposed geographic coordinates and standardizes country names
#'
#' Flags and corrects records when latitude and longitude appear to be 
#' transposed and standardizes country names.
#'
#' @param data data.frame. Containing an unique identifier for each records,
#' geographical coordinates, and country names.
#' @param id character string. The column with an unique record identifier.
#' Default = "database_id".
#' @param sci_names character string. The column with species scientific name.
#' Default = "scientificName".
#' @param lat character string. The column with latitude Default =
#' "decimalLatitude".
#' @param lon character string. The column with longitude. Default =
#' "decimalLongitude".
#' @param  country character string. The column with the country assignment of
#' each record. Default = "country".
#' @details This test identifies transposed coordinates resulted from mismatches
#' between the country informed to a record and coordinates. Transposed
#' coordinates often fall outside of the indicated country (i.e., in other
#' countries or in the sea). If 'country_from_xy' is TRUE country names
#' derived from valid coordinates is added to records missing country
#' information. Otherwise, only records with interpretable country names are
#' considered. First, country names are standardized using an exact matching
#' against a list of country names in several languages from Wikipedia. If
#' there any unmatched names remain, a fuzzy matching algorithm is used to
#' find potential candidates for each misspelled countries names.  Then,
#' standardized country names are used to identifying mismatches between
#' country and coordinates. To do this, the 'seas' and, 'countries' tests from
#' 'CoordinateCleaner' package are used. Once detected a mismatch, different
#' coordinate transformations are performed to correct country/coordinates
#' mismatch. Importantly, verbatim coordinates are replaced by the corrected
#' ones in the returned database. A database containing verbatim and corrected
#' coordinates is created in "Output/Check/01_transposed_xy.csv".
#' @return A data.frame containing three columns i) 'transposed_xy' (logical,
#'   records that have failed in the test are flagged as "FALSE", ii)
#'   country_suggested (standardized country names), and iii) country_code
#'   (standardized iso3 country code).
#'
#' @examples
#' id <- c(1,2,3,4)
#' scientificName <- c("Rhinella major", "Scinax ruber", 
#'                     "Siparuna guianensis", "Psychotria vellosiana")
#' decimalLatitude <- c(-63.43333, -67.91667, -41.90000, -46.69778)
#' decimalLongitude <- c(-17.90000, -14.43333, -13.25000, -13.82444)
#' country <- c("BOLIVIA", "bolivia", "Brasil", "Brazil")
#' 
#' x <- data.frame(id, scientificName, decimalLatitude,
#'                 decimalLongitude, country)
#' 
#' bdc_transposed_xy(
#'   data = x,
#'   id = "id",
#'   sci_names = "scientificName",
#'   lat = "decimalLatitude",
#'   lon = "decimalLongitude",
#'   country = "country"
#' )
bdc_transposed_xy <-
  function(data,
           id,
           sci_names,
           lat,
           lon,
           country,
           country_from_xy = TRUE) {
    
  minimum_colnames <- c(id, sci_names, lon, lat, country)

  if (length(minimum_colnames) < 5) {
    stop("Fill all function arguments: data, id, sci_names, lon, lat, and 
         country")
  }
  
  if (!all(minimum_colnames %in% colnames(data))) {
    stop(
      "These columns names were not found in your database: ",
      paste(minimum_colnames[!minimum_colnames %in% colnames(data)], 
            collapse = ", "),
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
  
  if (isTRUE(country_from_xy)){
    worldmap <- rnaturalearth::ne_countries(scale = "large")

    data_no_country <-
      data %>%
      dplyr::filter(is.na(country) | country == "")
    
    # converts coordinates columns to spatial points
    suppressWarnings({
    data_sp <-
      CoordinateCleaner::cc_val(
        x = data_no_country,
        lon = lon,
        lat = lat,
        verbose = F
      ) %>%
      CoordinateCleaner::cc_sea(
        x = .,
        lon = lon,
        lat = lat,
        verbose = F,
        speedup = TRUE,
        ref = worldmap
      ) %>%
      sf::st_as_sf(.,
                   coords = c("decimalLongitude", "decimalLatitude"),
                   remove = F) %>%
      sf::st_set_crs(., st_crs(worldmap))
    })
    
    worldmap <- sf::st_as_sf(worldmap) %>% dplyr::select(name_long)
    
    # Extract country names from coordinates
    suppressMessages({
      ext_country <- 
        data_sp %>% 
        dplyr::select(database_id, geometry) %>% 
        st_intersection(., worldmap)
    })
    
    ext_country$geometry <- NULL
    w <- which(data$database_id %in% ext_country$database_id)
    
    data[w, "country"] <- ext_country$name_long
  }
  
  # load auxiliary data
  message("Loading auxiliary data: country names from wikipedia\n")
  suppressMessages({
    wiki_cntr <-
      bdc_get_wiki_country() # get country names from Wikipedia
  })
  
  message("Loading auxiliary data: world map and country iso\n")
  worldmap <- bdc_get_world_map() # get world map and country iso
  
  # standardize the name of countries
  message("Standardizing country names\n")
  standard_country_names <-
    bdc_standardize_country(data = data,
                            country = country,
                            country_names_db = wiki_cntr)

  cntr <- "cntr_original"
  names(cntr) <- country
  data <-
    data %>%
    dplyr::left_join(standard_country_names, by = cntr)
  
  # Correct latitude and longitude transposed
  message("Correcting latitude and longitude transposed\n")
  corrected_coordinates <-
    bdc_correct_coordinates(
      data = data,
      x = lon,
      y = lat,
      sp = sci_names,
      id = id,
      cntr_iso2 = "cntr_iso2c",
      world_poly = worldmap,
      world_poly_iso = "iso2c"
    )

  # Exports a table with verbatim and transposed xy
  corrected_coordinates <- 
  corrected_coordinates %>%
    dplyr::select(
      {{ id }},
      {{ sci_names }},
      dplyr::contains("decimal"),
      cntr_suggested
    ) 
  
  corrected_coordinates %>%
    readr::write_csv(here::here("Output/Check/01_transposed_xy.csv"))
  
  # finding the position of records with lon/lat modified
  w <- which(data[, {{id}} ] %in% (corrected_coordinates %>% pull({{id}})))
  
  data[w, "decimalLatitude"] <- corrected_coordinates[, "decimalLatitude_modified"]
  data[w, "decimalLongitude"] <- corrected_coordinates[, "decimalLongitude_modified"]
  
  # Flags transposed coordinates
  data$transposed_xy <- TRUE
  data[w, "transposed_xy"] <- FALSE

  data <-
    data %>%
    dplyr::rename(
      country_suggested = cntr_suggested,
      countryCode = cntr_iso2c)

  message(
    paste(
      "\nbdc_transposed_xy:\nCorrected",
      sum(data$transposed_xy == FALSE),
      "records.\nThree columns were added to the database.\nCheck database
      containing coordinates corrected in: Output/Check/01_transposed_xy.csv\n"
    )
  )

  return(data)
}
