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
  name_en <- NULL

  check_require_cran("rnaturalearth")
  check_require_github("ropensci/rnaturalearthdata")

  suppressWarnings({
    worldmap <- rnaturalearth::ne_countries(scale = "large")

    # Add some iso code to some countries polygons
    iso2c <- countrycode::countrycode(unique(worldmap$name_en),
      origin = "country.name.en",
      destination = "iso2c"
    )

    iso3c <- countrycode::countrycode(unique(worldmap$name_en),
      origin = "country.name.en",
      destination = "iso3c"
    )

    iso <- data.frame(
      worldmap@data %>%
        dplyr::select(name_en, tidyselect::starts_with("iso")),
      iso2c,
      iso3c
    )

    filt <- !is.na(iso$iso_a2) & is.na(iso$iso2c)
    iso$iso2c[filt] <- iso$iso_a2[filt]

    filt <- !is.na(iso$iso_a3) & is.na(iso$iso3c)
    iso$iso3c[filt] <- iso$iso_a3[filt]

    worldmap@data <- iso
    is.na(iso) %>% colSums() # number of polygons without isocode

    worldmap@data <-
      worldmap@data %>%
      dplyr::select(iso2c, iso3c)
  })
  return(worldmap)
}
