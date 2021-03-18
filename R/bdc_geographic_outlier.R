#' Identify plant species records outside known geographic distribution
#'
#' Flags geographic outliers by comparing the status where record is against
#' species range (i.e., Brazilian states) obtained from the Flora do Brazil
#' 2020.
#'
#' @param data data.frame. A data frame containing geographic coordinates and species names.
#' @param species character string. The column with scientific names.
#' @param longitude character string. The column with longitude in decimal degrees. Default = "decimalLongitude"
#' @param latitude character string. The column with the latitude in decimal degrees. Default = "decimalLatitude".
#'
#' @details Check if coordinates are valid before run this test.
#' Species occurrence records in Goias and range in DF are assigned as TRUE.
#' Similarly, species occurrence in DF and range is GO are considered TRUE.
#' @return flag of records are outliers (FALSE) or not (TRUE)
#'
#' @export
#' @examples
#' \dontrun{
#' }
bdc_geographic_outlier <-
  function(data,
           species = "scientificName",
           longitude = "decimalLongitude",
           latitude = "decimalLatitude") {


    # brazilian states
    bra_states <-
      geobr::read_state(year = 2018, simplified = T) %>%
      dplyr::select(abbrev_state)

    data <-
      data %>%
      dplyr::mutate(id = 1:nrow(.))

    # convert to spatial
    df_spatial <-
      sf::st_as_sf(
        data = data,
        coords = c("longitude", "latitude"),
        crs = sf::st_crs(bra_states)
      ) %>%
      dplyr::select(id, species)


    # check the overlap between occurence records to Brazilian states
    df_spatial <-
      sf::st_intersection(df_spatial, bra_states) %>%
      dplyr::rename(record_occurence = abbrev_state)

    # get species range from Flora do Brasil 2020
    occ_states <-
      flora::get.taxa(
        taxa = df_spatial %>% pull(species),
        states = T
      ) %>%
      dplyr::select(original.search, occurrence) %>%
      dplyr::rename(
        species_range = occurrence,
        scientificName = original.search
      )

    df_spatial <-
      dplyr::full_join(df_spatial, occ_states, by = c("species" = "scientificName")) %>%
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
