# Load all function required
devtools::load_all()

# Load and install all packages required
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

# Define the path containing the configuration database
metadata <- data.table::fread(here::here("inst/extdata/Config/DatabaseInfo.csv"))

# Standardize and merge datasets  
bdc_standardize_datasets(metadata = metadata)
