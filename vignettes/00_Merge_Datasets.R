# Load all function required
devtools::load_all()

if (!require("here")) install.packages("here")

merged_filename <- here::here("Data", "temp", "standard_database.qs")

if (!file.exists(merged_filename)) {
  
  ipak(
    c(
      "tidyverse",
      "here",
      "glue",
      "fs",
      "janitor",
      "vroom",
      "waldo",
      "tidylog",
      "qs",
      "data.table"
    )
  )
  
  metadata <- data.table::fread(here::here("Config/DatabaseInfo.csv"))
  
  bdc_standardize_datasets(metadata = metadata)
  
  # Concatenate all the resulting standardized databases
  merged_database <-
    here::here("data", "temp") %>%
    fs::dir_ls(regexp = "*.qs") %>% 
    plyr::ldply(.data = .,
                .fun = qread,
                .progress = plyr::progress_text(char = "."), 
                .id = NULL)
  
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
    qs::qsave(merged_filename)
  
} else {
  
  message(paste(merged_filename, "already exists!"))
  
}
