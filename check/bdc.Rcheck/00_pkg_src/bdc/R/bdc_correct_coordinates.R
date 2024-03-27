#' Internal function. Detects and corrects transposed geographic coordinates
#'
#' This functions detects mismatches between country names informed coordinates.
#' Once detects, transposed coordinates are corrected by the used of different
#' coordinates transformations by using the 'bdc_coord_trans' function.
#'
#' @param data data.frame. Containing an unique identifier for each records,
#' geographical coordinates, and country names. Coordinates must be expressed in
#' decimal degree and in WGS84.
#' @param x character string. The column name with longitude. Default =
#' "decimalLongitude".
#' @param y character string. The column name with latitude Default =
#' "decimalLatitude".
#' @param sp character string. The column name with species scientific name.
#' Default = "scientificName".
#' @param id id character string. The column name with an unique record
#' identifier. #' Default = "id".
#' @param cntr_iso2 character string. The column name with the country code
#' assignment of each record. Default = "country_code".
#' @param world_poly polygon. Borders of the world.
#' @param world_poly_iso charterer sting. Iso2 code column of country polygon
#' database
#' @param border_buffer numeric. A distance in decimal degrees used to created a
#' buffer around the country. Records within a given country and at a specified
#' distance from its coast will be not be corrected. Default = 0.2 (~20 km at
#' the equator).
#'
#'
#' @importFrom CoordinateCleaner cc_val clean_coordinates
#' @importFrom dplyr filter mutate as_tibble select all_of pull bind_rows distinct relocate left_join
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#'
#' }
bdc_correct_coordinates <-
  function(data,
           x,
           y,
           sp,
           id,
           cntr_iso2,
           world_poly,
           world_poly_iso,
           border_buffer = border_buffer) {
    . <- decimalLatitude <- decimalLongitude <- .summary <- NULL

    x_mod <- paste0(x, "_modified")
    y_mod <- paste0(y, "_modified")

    occ_country <- data %>% dplyr::filter(c(!is.na(data[,cntr_iso2])))

    # Filter occurrences database to avoid error in clean_coordinates errors
    suppressWarnings({
      suppressMessages({
        occ_country <-
          occ_country %>%
          CoordinateCleaner::cc_val(., lon = x, lat = y) %>%
          dplyr::mutate(
            decimalLatitude = as.numeric(decimalLatitude),
            decimalLongitude = as.numeric(decimalLongitude)
          )
      })
    })

    # Detect records outside a country
    suppressWarnings({
      suppressMessages({
        occ_country <- CoordinateCleaner::clean_coordinates(
          x = occ_country,
          lon = x,
          lat = y,
          species = sp,
          countries = cntr_iso2,
          # iso2 code column name
          # testing records in the sea and outside georeferenced countries
          tests = c("seas", "countries"),
          # high-quality countries border database
          country_ref = world_poly,
          # iso2 code column of country polygon database
          country_refcol = world_poly_iso,
          seas_ref = world_poly,
          value = "spatialvalid"
        )
      })
    })

    
    # Separate those records outside their countries
    occ_country <-
      occ_country %>%
      dplyr::as_tibble() %>%
      dplyr::filter(!.summary, c(!is.na(occ_country[cntr_iso2])))

    # now this database have all those records with potential error that be
    # corrected
    message(occ_country %>% nrow(), " occurrences will be tested")
    
    # If occ_country have no data 
    if(nrow(occ_country)==0){
      return(NULL)
    }
    
    # Split database
    occ_country <-
      split(occ_country, occ_country[cntr_iso2])


    # bdc_coord_trans() function will try different coordinate transformations
    # to correct georeferenced occurrences
    coord_test <- list()

    for (i in 1:length(occ_country)) {
      message(
        "Processing occurrences from: ",
        occ_country[[i]][cntr_iso2] %>% unique(),
        paste0(" (", nrow(occ_country[[i]]), ")")
      )
      try(coord_test[[i]] <-
        bdc_coord_trans(
          data = occ_country[[i]],
          x = x,
          y = y,
          country_code = cntr_iso2,
          id = id,
          worldmap = world_poly,
          worldmap_cntr_code = world_poly_iso
        ))
    }

    # elimination from the list those countries without correction
    filt <- sapply(coord_test, function(x) nrow(x) > 0)
    
    if(any(filt)){
    coord_test <- coord_test[filt]

    # Elimination of those records near to country border (to avoid flip
    # coordinates or sign that fall too close to country border)

    for (i in 1:length(coord_test)) {
      n <-
        coord_test[[i]] %>%
        dplyr::select(dplyr::all_of(cntr_iso2)) %>%
        unique() %>%
        dplyr::pull()

      # Here filter polygon based on your country iso2c code
      my_country <-
        world_poly[which(world_poly@data[, world_poly_iso] == n), ]

      # 0.5 degree ~50km near to equator
      my_country2 <- raster::buffer(my_country, width = border_buffer)

      coord_sp <- sp::SpatialPoints(coord_test[[i]] %>% dplyr::select({{ x }}, {{ y }}))

      coord_sp@proj4string <- my_country2@proj4string
      over_occ <- sp::over(coord_sp, my_country2)

      # Eliminate as corrected those records too close to country border
      coord_test[[i]] <-
        coord_test[[i]] %>% dplyr::filter(is.na(over_occ))
    }

    # Elimination of those records with more than two possible correction
    coord_test <-
      dplyr::bind_rows(coord_test) %>%
      dplyr::as_tibble() # binding dataframes allocated in the list in a single one

    coord_test <-
      coord_test[!duplicated(coord_test[id]), ] %>%
      dplyr::relocate(dplyr::all_of(id), dplyr::all_of(x), dplyr::all_of(y))

    # Merge coord_test with other columns of occurrence database
    coord_test <-
      dplyr::left_join(coord_test,
        data %>% dplyr::select(-c({{ x }}, {{ y }}, {{ cntr_iso2 }})),
        by = id
      )

    return(coord_test)
    }else{
      return(NULL)
    }
  }
