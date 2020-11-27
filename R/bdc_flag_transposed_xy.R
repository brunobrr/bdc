# TODO: run prefixer to catch namespaces and importFrom

#' Flag transposed coordinates
#'
#' @description
#' This function add a new column `.transposed_xy` in the returned dataset
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
bcd_flag_transposed_xy <- function(data) {

  minimum_colnames <- c("database_id", "scientificName", "decimalLongitude", "decimalLatitude")
  check_minimum_colnames <-
    data %>%
    dplyr::select(minimum_colnames) %>% {
      names(.) %in% minimum_colnames
    }

  if (sum(check_minimum_colnames) < length(check_minimum_colnames)) {
    stop(
      "Your data does not have the minimum default columns names: ",
      paste(minimum_colnames, collapse = ", "), call. = FALSE
    )
  }


  # load auxiliar data
  wiki_cntr <- bdc_get_wiki_country() # get country names from Wikipedia
  worldmap <- bdc_get_world_map()  # 

  # standardize the name of countries
  standard_country_names <-
    bdc_standardize_country(
      data = data,
      country = "country",
      country_names_db = wiki_cntr
    )

  data <-
    data %>%
    dplyr::left_join(standard_country_names, by = c("country" = "cntr_original"))

  # Correct latitude and longitude transposed
  corrected_coordinates <-
    bdc_correct_coordinates(
      data = data,
      x = "decimalLongitude",
      y = "decimalLatitude",
      sp = "scientificName",
      id = "database_id",
      cntr_iso2 = "cntr_iso2c",
      world_poly = worldmap,
      world_poly_iso = "iso2c"
    )

  rows_to_remove <-
    corrected_coordinates %>%
    dplyr::pull(database_id)

  rows_to_insert <-
    corrected_coordinates %>%
    # remove columns with coordinates transposed
    dplyr::select(-decimalLatitude, -decimalLongitude) %>%
    # new columns coordinates with the corrected info
    dplyr::rename(decimalLatitude = decimalLatitude_modified,
                  decimalLongitude = decimalLongitude_modified) %>%
    # flag all of them
    dplyr::mutate(.transposed_xy = TRUE)

  data <-
    data %>%
    # remove wrong coordinates
    dplyr::filter(!database_id %in% rows_to_remove) %>%
    # flag no issued rows as FALSE
    dplyr::mutate(.transposed_xy = FALSE) %>%
    # add corrected coordinates
    dplyr::bind_rows(rows_to_insert)

  # save issued coordinates
  message("Saving issued coordinates in Output/Check/01_prefilter_transposed_coordinates.csv")

  corrected_coordinates %>%
    dplyr::select(
      database_id,
      scientificName,
      dplyr::contains("decimal"),
      locality,
      stateProvince,
      cntr_suggested
    ) %>%
    readr::write_csv(here::here("Output", "Check", "01_prefilter_transposed_coordinates.csv"))

  return(data)

}
