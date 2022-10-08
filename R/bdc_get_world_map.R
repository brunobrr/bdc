#' Internal function. Get a world map with country names and iso code
#'
#' This is a helper function used to obtain names, iso code, and the limits
#' (polygon) of world countries. Data from the package 'rnaturalearth'.
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' worldmap <- bdc_get_world_map()
#' }
bdc_get_world_map <- function() {
  name_long <- english_name <- english_name <- iso2c <- iso_a2 <- geometry <- alpha2 <- alpha3 <- cntr_original2 <- NULL

  check_require_cran("rnaturalearth")
  check_require_github("ropensci/rnaturalearthdata")

  suppressWarnings({

    cntr_names <-
      system.file("extdata/countries_names/country_names.txt", package = "bdc") %>%
      readr::read_delim(delim = "\t", col_types = readr::cols()) %>%
      ## FIXME 2022-10-08: There are two cases as "United States".
      dplyr::mutate(english_name = dplyr::if_else(alpha3 == "USA", "United States of America", english_name)) %>%
      dplyr::select(english_name, iso2c = alpha2) %>%
      unique()

    worldmap <-
      rnaturalearth::ne_countries(scale = "large", returnclass = "sf") %>%
      ## correct country names to match our database `cntr_names`
      bdc_reword_countries() %>%
      dplyr::left_join(cntr_names, by = c("name_long" = "english_name")) %>%
      ## replace NA on our database with iso names on rnaturalearth (two cases: Namibia and Kosovo).
      ## NOTE: the iso2c for Namibia is the string "NA".
      dplyr::mutate(iso2c = if_else(!is.na(iso_a2) & is.na(iso2c), iso_a2, iso2c)) %>%
      dplyr::select(name_long, iso2c, geometry) %>%
      ## return SpatialPolygonDataFrame
      sf::as_Spatial()

  })

  return(worldmap)

}
