# TODO: run prefixer to catch namespaces and importFrom

#' @titile 
#' Flag transposed coordinates
#'
#' @description
#' This function add a new column `transposed_xy` in the returned dataset
#'
#' @param data a data.frame with the default column names: "database_id", "scientificName", "decimalLongitude", "decimalLatitude"
#' 
#' @importFrom dplyr select left_join pull rename mutate filter bind_rows contains
#' @importFrom here here
#' @importFrom readr write_csv
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' data %>%
#'   bdc_flag_transposed_xy()
#' }

bdc_flag_transposed_xy <- function(data, id, sci_name, lon, lat, country) {
  
  minimum_colnames <- c(id, sci_name, lon, lat, country)
  
  if (length(minimum_colnames) < 5) {
    stop("Fill all function arguments: data, id, sci_name, lon, lat, and country")
  }
  
  if (!all(minimum_colnames %in% colnames(data))) {
    stop(
      "These columns names were not found in your database: ",
      paste(minimum_colnames[!minimum_colnames %in% colnames(data)], collapse = ", "),
      call. = FALSE
    )
  }
  
  # load auxiliary data
  message('Loading auxiliary data: country names from wikipedia\n')
  suppressMessages({
  wiki_cntr <- bdc_get_wiki_country() # get country names from Wikipedia
  })
  
  message('Loading auxiliary data: world map and country iso\n')
  worldmap <- bdc_get_world_map()  # get world map and country iso

  
  # standardize the name of countries
  message('Standardizing country names\n')
  standard_country_names <-
    bdc_standardize_country(
      data = data,
      country = country,
      country_names_db = wiki_cntr
    )
  
  cntr <- 'cntr_original'
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
      sp = sci_name,
      id = id,
      cntr_iso2 = "cntr_iso2c",
      world_poly = worldmap,
      world_poly_iso = "iso2c"
    )
  
  rows_to_remove <-
    corrected_coordinates %>%
    dplyr::pull({{id}})
  
  rows_to_insert <-
    corrected_coordinates %>%
    # remove columns with coordinates transposed
    dplyr::select(-{{lat}}, -{{lon}}) 
  
  # new columns coordinates with the corrected info
  colnames(rows_to_insert)[
    colnames(rows_to_insert) %in% 
      c('decimalLatitude_modified', 'decimalLongitude_modified')] <- c(lat, lon)
  
  # flag all of them
  rows_to_insert <- rows_to_insert %>% dplyr::mutate(transposed_xy = FALSE)
  
  data <-
    data %>%
    # remove wrong coordinates
    dplyr::filter(!(!!rlang::sym(id) %in% rows_to_remove)) %>%
    # flag no issued rows as TRUE
    dplyr::mutate(transposed_xy = TRUE) %>%
    # add corrected coordinates
    dplyr::bind_rows(rows_to_insert) %>% 
    dplyr::rename(country_suggested = cntr_suggested,
                  country_iso2c = cntr_iso2c,
                  transposed_xy = transposed_xy) %>% 
    dplyr::select(-c(iso2c, iso3c))

  corrected_coordinates %>%
    dplyr::select(
      {{id}},
      {{sci_name}},
      {{lon}},
      {{lat}},
      dplyr::contains("decimal"),
      cntr_suggested
    ) %>%
    readr::write_csv(here::here("Output/Check/01_transposed_xy.csv"))
  
  message(
    paste(
      "\nbdc_flag_transposed_xy:\nCorrected",
      sum(data$transposed_xy == FALSE),
      "records.\nThree columns were added to the database.\nCheck database containing coordinates corrected in: Output/Check/01_transposed_xy.csv\n"))
  
  return(data)
}
