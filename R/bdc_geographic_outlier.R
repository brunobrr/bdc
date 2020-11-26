#' Title Flag geographic outliers by comparing the status where record is against species range (i.e., Brazilian states) obtained from the Flora do Brazil 2020. 
#'
#' @param x Data.frame containg geographic coordinates and species names.
#' @param species Character string. The column with the species name
#' @param decimalLongitude. Numeric. The column with longitude in decimal degrees. Dafault = "decimalLongitude"
#' @param decimalLatitude. Numeric. The column with the latitude in decimal degrees. Dafault = "decimalLatitude".
#' @details Check if coordinates are valid before run this test. Species ccurrence records in Goias and range in DF are assinged as TRUE. Similarly, species occurence in DF and range is GO are considered TRUE. 
#' 
#' 
#'
#' @return flag of records are outliers (FALSE) or not (TRUE)
#' @export
#'
#' @examples
bdc_geographic_outlier <-
  function(x,
           species = "scientificName",
           longitude = "decimalLongitude",
           latitude = "decimalLatitude") {
    
    
    # brazilian states
    bra_states <-
      geobr::read_state(year = 2018, simplified = T) %>%
      dplyr::select(abbrev_state)
    
    x <- 
      x %>% 
      dplyr::mutate(id = 1:nrow(x))
    
    # convert to spatial
    df_spatial <-
      sf::st_as_sf(
        x = x,
        coords = c(longitude, latitude),
        crs = st_crs(bra_states)
      ) %>%
      dplyr::select(id, species)
    
    
    # check the overlap between occurence records to Brazilian states
    df_spatial <-
      st_intersection(df_spatial, bra_states) %>%
      dplyr::rename(record_occurence = abbrev_state)
    
    # get species range from Flora do Brasil 2020
    occ_states <-
      flora::get.taxa(
        taxa = df_spatial %>% pull(species),
        states = T
      ) %>%
      dplyr::select(original.search, occurrence) %>%
      dplyr::rename(species_range = occurrence, 
                    scientificName = original.search)
    
    df_spatial <-
      dplyr::full_join(df_spatial, occ_states) %>%
      dplyr::distinct(id, .keep_all = T)
    
    df_spatial$geometry <- NULL
    
    df <-
      df_spatial %>% dplyr::select(-scientificName) %>% 
      dplyr::full_join(x, ., by = "id")
    
    
    # Check whether the records are whithin species range
    df <-
      df %>%
      mutate(.geographic_outlier = stringr::str_detect(species_range, record_occurence))
    
    
    # Convert as TRUE points in GO but range is only DF
    go_in_df <- which(df$record_occurence == "DF" &
                        str_detect(string = df$species_range, pattern = "GO"))
    
    # Convert as TRUE points in DF and range is GO state
    df_in_go <- which(df$record_occurence == "GO" &
                        str_detect(string = df$species_range, pattern = "DF"))
    
    df[go_in_df, ".geographic_outlier"] <- TRUE
    df[df_in_go, ".geographic_outlier"] <- TRUE
    
    df <- 
      df %>% 
      dplyr::select(-c(id, species_range, record_occurence))
    
    return(df)
  }
