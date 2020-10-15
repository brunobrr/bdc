# ipak function: install and load multiple R packages.
# Check to see if packages are installed.
# Install them if they are not, then load them into the R session.
# Forked from: https://gist.github.com/stevenworthington/3178163
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

metadata <- read_csv(here::here("Config/DatabaseInfo.csv"))

standardize_dataset <- function(metadata) {

  metadata <-
    metadata %>%
    clean_names()

  save_in_dir <- here::here("data", "temp")

  if (!dir_exists(save_in_dir)) {
    dir_create(save_in_dir)
  }

  input_file <-
    metadata %>%
    pull(file_name_to_load)

  for (file_index in seq_along(input_file)) {

    dataset_name <-
      metadata %>%
      filter(file_name_to_load == input_file[file_index]) %>%
      select(dataset_name) %>%
      pull()

    save_in_filename <- paste0(save_in_dir, "/standard_", dataset_name, ".csv")

    if (!file.exists(save_in_filename)) {

      # FIXME: for now, just remove these columns
      problem <- c("basis_of_record", "basisofrecord", "locality_site", "locality")

      dwc_names <-
        metadata %>%
        filter(file_name_to_load == input_file[file_index]) %>%
        clean_names() %>%
        select_if(~ !is.na(.)) %>%
        select(-dataset_name, -file_name_to_load, -any_of(problem)) %>%
        names(.)

      vector_recode <-
        metadata %>%
        filter(file_name_to_load == input_file[file_index]) %>%
        clean_names() %>%
        select_if(~ !is.na(.)) %>%
        select(-dataset_name, -file_name_to_load, -any_of(problem)) %>%
        make_clean_names() %>%
        set_names(dwc_names)

      standard_dataset <-
        readr::read_csv(input_file[file_index], guess_max = 10^6) %>%
        clean_names() %>%
        select(all_of(vector_recode)) %>%
        set_names(names(vector_recode)) %>%
        mutate(database_id = paste0(dataset_name, "_", 1:n())) %>%
        select(database_id, everything())

      standard_dataset %>%
        select(database_id, scientific_name, decimal_latitude, decimal_longitude) %>%
        write_csv(save_in_filename)

    } else {

      message(glue("{save_in_filename} already exists!"))

    }

  }

}

standardize_dataset(metadata = metadata)

here::here("data", "temp") %>%
  fs::dir_ls(regexp = "*.csv") %>%
  map_dfr(
    ~ read_csv(
        file = .x,
        guess_max = 10^6,
        col_types = cols(
          database_id       = col_character(),
          scientific_name   = col_character(),
          decimal_latitude  = col_double(),
          decimal_longitude = col_double()
        )
      )
  )
