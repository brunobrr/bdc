# Testing standardize_dataset function ----------------------------------------
if (!require("here")) install.packages("here")

merged_filename <- here::here("data", "temp", "standard_database.xz")

if (!file.exists(merged_filename)) {

  source(here::here("R/aux_functions.R"))

  ipak(
    c(
      "tidyverse",
      "here",
      "glue",
      "fs",
      "janitor",
      "vroom",
      "waldo",
      "tidylog"
    )
  )

  metadata <- readr::read_csv(here::here("Config/DatabaseInfo.csv"))

  bdc_standardize_dataset(metadata = metadata)

  # Testing if vroom can concatenate all the resulting standandized databases ---
  merged_database <-
    here::here("data", "temp") %>%
    fs::dir_ls(regexp = "*.xz") %>%
    purrr::map_dfr(
      ~ vroom::vroom(
          file = .x,
          guess_max = 10^6,
          col_types = cols(.default = "c") #,
        )
    )

  merged_database %>%
    mutate(database_name = str_remove(database_id, "_[0-9].*")) %>%
    distinct(database_name)

  waldo::compare(
    x = merged_database %>% names(),
    y = metadata %>% names()
    )

  merged_database<-
    merged_database %>% 
    select_if((function(x) any(!is.na(x))))
  
  merged_database %>%
    vroom::vroom_write(merged_filename)

} else {

  message(paste(merged_filename, "already exists!"))

}

