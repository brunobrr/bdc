#' Identify plant species records outside their known geographic distribution
#'
#' This function flags records outside their known geographical distribution
#' (i.e., Brazilian states). Geographical distribution information is obtained
#' from Flora do Brasil 2020.
#'
#' @param data data.frame. A data frame containing geographic coordinates and
#' scientific names.
#' @param sci_names character string. The column name with species scientific
#' name. Default = "scientificName".
#' @param lat character string. The column name with latitude. Coordinates must
#' be expressed in decimal degree and in WGS84. Default = "decimalLatitude".
#' @param lon character string. The column with longitude. Coordinates must be
#' expressed in decimal degree and in WGS84. Default = "decimalLongitude".
#'
#' @details Check if coordinates are valid before run this test.
#' Species occurrence records in Goias and range in DF are assigned as TRUE.
#' Similarly, species occurrence in DF and range is GO are considered TRUE.
#'
#' @return A data.frame contain the column ".otl". Compliant
#' (TRUE) if 'lat' and 'lon' are not empty; otherwise "FALSE".
#'
#' @export
bdc_geographic_outlier <-
  function(data,
           sci_names = "scientificName",
           lat = "decimalLatitude",
           lon = "decimalLongitude") {

    check_require_cran("geobr")

    # brazilian states
    bra_states <-
      geobr::read_state(year = 2018, simplified = T) %>%
      dplyr::select(abbrev_state)

    data <-
      data %>%
      dplyr::mutate(id = 1:nrow(.)) %>%
      dplyr::mutate(
        decimalLatitude = as.numeric(.data[[lat]]),
        decimalLongitude = as.numeric(.data[[lon]])
      )

    # convert to spatial
    suppressWarnings({
      data_sp <-
        CoordinateCleaner::cc_val(
          x = data,
          lon = lon,
          lat = lat,
          verbose = FALSE
        ) %>%
        sf::st_as_sf(.,
                     coords = c("decimalLongitude", "decimalLatitude"),
                     remove = FALSE
        ) %>%
        sf::st_set_crs(., sf::st_crs(bra_states))
    })


    # check the overlap between occurence records to Brazilian states
    data_sp <-
      sf::st_intersection(data_sp, bra_states) %>%
      dplyr::rename(record_occurence = abbrev_state)

    # get sci_names range from Flora do Brasil 2020
    occ_states <-
      flora::get.taxa(
        taxa = data_sp %>% pull(sci_names),
        states = T
      ) %>%
      dplyr::select(original.search, occurrence) %>%
      dplyr::rename(
        species_range = occurrence,
        scientificName = original.search
      )

    df_spatial <-
      dplyr::full_join(data_sp, occ_states,
                       by = c("species" = "scientificName")) %>%
      dplyr::distinct(id, .keep_all = TRUE)

    df_spatial$geometry <- NULL

    df <-
      df_spatial %>%
      dplyr::full_join(data, ., by = "species")


    # Check whether the records are whithin species range
    df <-
      df %>%
      mutate(
        .geographic_outlier = stringr::str_detect(
          species_range,
          record_occurence
        )
      )


    # Convert as TRUE points in GO but range is only DF
    go_in_df <- which(df$record_occurence == "DF" &
      stringr::str_detect(string = df$species_range, pattern = "GO"))

    # Convert as TRUE points in DF and range is GO state
    df_in_go <- which(df$record_occurence == "GO" &
      stringr::str_detect(string = df$species_range, pattern = "DF"))

    df[go_in_df, ".geographic_outlier"] <- TRUE
    df[df_in_go, ".geographic_outlier"] <- TRUE

    df <-
      df %>%
      dplyr::select(-c(contains("id."), species_range, record_occurence)) %>%
      dplyr::mutate(.geographic_outlier = dplyr::if_else(is.na(.geographic_outlier), TRUE, .geographic_outlier))

    return(df)

  }
