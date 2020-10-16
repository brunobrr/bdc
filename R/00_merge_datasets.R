#' Standardize datasets columns based on metadata file
#'
#' @param metadata a table containing information about which columns of the 
#'   original dataset need to be renamed following the names adopted by GBIF. 
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

  metadata <-
    metadata %>%
    janitor::clean_names()

  save_in_dir <- here::here("data", "temp")

  if (!fs::dir_exists(save_in_dir)) {
    fs::dir_create(save_in_dir)
  }

  input_file <-
    metadata %>%
    dplyr::pull(file_name_to_load)

  for (file_index in seq_along(input_file)) {

    dataset_name <-
      metadata %>%
      dplyr::filter(file_name_to_load == input_file[file_index]) %>%
      dplyr::select(dataset_name) %>%
      dplyr::pull()

    save_in_filename <- paste0(save_in_dir, "/standard_", dataset_name, ".csv")

    if (!file.exists(save_in_filename)) {

      # FIXME: for now, just remove these columns
      problem <- c("basis_of_record", "basisofrecord", "locality_site", "locality")

      basename_names <-
        metadata %>%
        dplyr::filter(file_name_to_load == input_file[file_index]) %>%
        janitor::clean_names() %>%
        dplyr::select_if(~ !is.na(.)) %>%
        dplyr::select(-dataset_name, -file_name_to_load, -any_of(problem))

      standard_names <-
        basename_names %>%
        names(.)

      vector_for_recode <-
        basename_names %>%
        janitor::make_clean_names() %>%
        purrr::set_names(standard_names)

      standard_dataset <-
        here::here(input_file[file_index]) %>%
        readr::read_csv(guess_max = 10^6) %>%
        janitor::clean_names() %>%
        dplyr::select(all_of(vector_for_recode)) %>%
        purrr::set_names(names(vector_for_recode)) %>%
        dplyr::mutate(database_id = paste0(dataset_name, "_", 1:dplyr::n())) %>%
        dplyr::select(database_id, dplyr::everything())

      standard_dataset %>%
        dplyr::select(database_id, scientific_name, decimal_latitude, decimal_longitude) %>%
        readr::write_csv(save_in_filename)

    } else {

      message(glue::glue("{save_in_filename} already exists!"))

    }

  }

}

# Testing ----------------------------------------------------------------------
ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) {
    install.packages(new.pkg, dependencies = TRUE)
  }
  suppressPackageStartupMessages(sapply(pkg, require, character.only = TRUE))
}

ipak(
  c(
    "tidyverse",
    "here",
    "glue",
    "fs",
    "janitor"
  )
)

metadata <- readr::read_csv(here::here("Config/DatabaseInfo.csv"))

standardize_dataset(metadata = metadata)

here::here("data", "temp") %>%
  fs::dir_ls(regexp = "*.csv") %>%
  purrr::map_dfr(
    ~ readr::read_csv(
        file = .x,
        guess_max = 10^6,
        col_types = readr::cols(
          database_id       = readr::col_character(),
          scientific_name   = readr::col_character(),
          decimal_latitude  = readr::col_double(),
          decimal_longitude = readr::col_double()
        )
      )
  )
