#' Standardize datasets columns based on metadata file
#'
#' @param metadata a table containing information about which columns of the
#'   original dataset need to be renamed following Darwin Core terminology.
#'   Please see the `Config/DatabaseInfo.csv` file.
#'
#' @importFrom dplyr pull filter select select_if mutate n everything
#' @importFrom fs dir_exists dir_create
#' @importFrom glue glue
#' @importFrom here here
#' @importFrom janitor clean_names make_clean_names
#' @importFrom purrr set_names
#' @importFrom readr read_csv write_csv
#'
#' @export
standardize_dataset <- function(metadata) {

  save_in_dir <- here::here("data", "temp")

  if (!fs::dir_exists(save_in_dir)) {
    fs::dir_create(save_in_dir)
  }

  input_file <-
    metadata %>%
    dplyr::pull(File_name_to_load)

  for (file_index in seq_along(input_file)) {

    dataset_name <-
      metadata %>%
      dplyr::filter(File_name_to_load == input_file[file_index]) %>%
      dplyr::select(datasetName) %>%
      dplyr::pull()

    save_in_filename <- paste0(save_in_dir, "/standard_", dataset_name, ".xz")

    if (!file.exists(save_in_filename)) {

      basename_names <-
        metadata %>%
        dplyr::filter(File_name_to_load == input_file[file_index]) %>%
        dplyr::select_if(~ !is.na(.)) %>%
        dplyr::select(-datasetName, -File_name_to_load)

      standard_names <-
        basename_names %>%
        names(.)

      vector_for_recode <-
        basename_names %>%
        purrr::set_names(standard_names) %>%
        { c(.) } %>%
        unlist()

      standard_dataset <-
        here::here(input_file[file_index]) %>%
        vroom::vroom(guess_max = 10^6, col_types = cols(.default = "c")) %>%
        dplyr::select(all_of(vector_for_recode)) %>%
        purrr::set_names(names(vector_for_recode)) %>%
        dplyr::mutate(database_id = paste0(dataset_name, "_", 1:dplyr::n())) %>%
        dplyr::select(database_id, dplyr::everything())

      standard_dataset %>%
        # # NOTE: comment out the line below to store each databse with standard columns
        # dplyr::select(database_id, scientific_name, decimal_latitude, decimal_longitude) %>%
        vroom::vroom_write(save_in_filename)
    } else {

      message(glue::glue("{save_in_filename} already exists!"))

    }

  }

}

# Testing standardize_dataset function ----------------------------------------
source(here::here("R/aux_functions.R"))

ipak(
  c(
    "tidyverse",
    "here",
    "glue",
    "fs",
    "janitor",
    "vroom",
    "waldo"
  )
)

metadata <- readr::read_csv(here::here("Config/DatabaseInfo.csv"))

standardize_dataset(metadata = metadata)

# Testing if vroom can concatenate all the resulting standandized databases ---
merged_database <-
  here::here("data", "temp") %>%
  fs::dir_ls(regexp = "*.xz") %>%
  purrr::map_dfr(
    ~ vroom::vroom(
        file = .x,
        guess_max = 10^6,
        col_types = readr::cols(
          # # NOTE: adjust the specification for each columns; col_character
          # #       for all columns is a trick.
          database_id                      = readr::col_character(),
          occurrence_id                    = readr::col_double(),
          scientific_name                  = readr::col_character(),
          decimal_latitude                 = readr::col_double(),
          decimal_longitude                = readr::col_double(),
          event_date                       = readr::col_date(),
          family                           = readr::col_character(),
          country                          = readr::col_character(),
          state_province                   = readr::col_character(),
          county                           = readr::col_character(),
          coordinate_precision             = readr::col_character(),
          taxon_rank                       = readr::col_character(),
          identified_by                    = readr::col_character(),
          coordinate_uncertainty_in_meters = readr::col_character(),
          recorded_by                      = readr::col_character()
        )
      )
  )

merged_database %>%
  mutate(database_name = str_remove(database_id, "_[0-9].*")) %>%
  distinct(database_name)

waldo::compare(
  x = merged_database %>% names(),
  y = metadata %>% names() %>% make_clean_names()
  )

merged_database %>% 
vroom::vroom_write(paste0(save_in_dir, "/standard_database", ".xz"))
