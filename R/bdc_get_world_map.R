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
  name_long <- NULL

  check_require_cran("rnaturalearth")
  check_require_github("ropensci/rnaturalearthdata")

  suppressWarnings({
    worldmap <- rnaturalearth::ne_countries(scale = "large")
    worldmap$name_long[grep("eSwatini", worldmap$name_long)] <-
      "Eswatini"
    worldmap$name_long[grep("Faeroe Islands", worldmap$name_long)] <-
      "Faroe Islands"
    worldmap$name_long[grep("Heard I. and McDonald Islands", worldmap$name_long)] <-
      "Heard Island and McDonald Islands"
    worldmap$name_long[grep("Indian Ocean Territories", worldmap$name_long)] <-
      "Australian Indian Ocean Territories"
    worldmap$name_long[grep("Lao", worldmap$name_long)] <-
      "Lao People's Democratic Republic"
    worldmap$name_long[grep("Pitcairn Island", worldmap$name_long)] <-
      "Pitcairn"
    worldmap$name_long[grep("Saint-Barthélemy", worldmap$name_long)] <-
      "Saint Barthélemy"
    worldmap$name_long[grep("Saint-Martin", worldmap$name_long)] <-
      "Saint Martin"
    worldmap$name_long[grep("South Georgia and the Islands", worldmap$name_long)] <-
      "South Georgia and the South Sandwich Islands"
    worldmap$name_long[grep("Gambia", worldmap$name_long)] <-
      "Gambia"
    worldmap$name_long[grep("Minor", worldmap$name_long)] <-
      "United States"
    worldmap$name_long[grep("Wallis and Futuna Islands", worldmap$name_long)] <-
      "Wallis and Futuna"
    
    
    # Add some iso code to some countries polygons
    iso2c <- countrycode::countrycode(worldmap$name_long,
      origin = "country.name.en",
      destination = "iso2c"
    )

    iso3c <- countrycode::countrycode(worldmap$name_long,
      origin = "country.name.en",
      destination = "iso3c"
    )

    iso <- data.frame(
      worldmap@data %>%
        dplyr::select(name_long, tidyselect::starts_with("iso")),
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
